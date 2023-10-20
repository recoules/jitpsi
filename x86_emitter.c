#define CAML_INTERNALS
#include <caml/mlvalues.h>
#include <caml/osdeps.h>
#include "caml/alloc.h"
#include "caml/stack.h"
#include <caml/fail.h>
#include <caml/custom.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

static size_t total_alloc = 0;
static size_t live_alloc = 0;

struct code_page {
  void *addr;
  uint32_t size;
  uint32_t offset;
  uint32_t refcnt;
};

struct root {
  struct code_page *page;
  void *frametable;
};

static struct code_page *last_page = NULL;


/* fix for 4.14.0 */
#define FIX_4_14_0 1

#ifdef FIX_4_14_0
static void *last_frametable = NULL;
static void *pending_frametable = NULL;
#endif

static void finalize (value vt)
{
  struct root *root = Data_custom_val(vt);
  struct code_page *page = root->page;
  void *frametable = root->frametable;
  root->page == NULL;
  root->frametable == NULL;
  if (page != NULL) {
    page->refcnt -= 1;
    if (page->refcnt == 0) {
      if (page == last_page) last_page = NULL;
      mprotect(page->addr, page->size, PROT_READ | PROT_WRITE);
      free(page->addr);
      live_alloc -= page->size;
      free(page);
    }
  }
  if (frametable != NULL) {
#ifdef FIX_4_14_0
    if (frametable == last_frametable) {
      pending_frametable = frametable;
      return;
    }
#endif
    caml_unregister_frametable(frametable);
    free(frametable);
  }
}

static struct custom_operations jitpsi_ops =
  {
    "https://github.com/recoules/jitpsi",
    finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
  };

CAMLprim value
ocaml_jitpsi_create (void)
{
  value vt = caml_alloc_custom(&jitpsi_ops, sizeof(struct root), 0, 1);
  struct root *root = Data_custom_val(vt);
  root->page = NULL;
  root->frametable = NULL;
  return vt;
}

CAMLprim intnat
native_jitpsi_load_code (value vbuf, intnat size, intnat align, value vt)
{
  struct code_page *page;
  if ((last_page == NULL) ||
      (last_page->size < last_page->offset + align + size)) {
    if (last_page != NULL)
      mprotect(last_page->addr, last_page->size, PROT_READ | PROT_EXEC);
    page = malloc(sizeof(struct code_page));
    long pagesize = sysconf(_SC_PAGESIZE);
    long numpages = (size + pagesize - 1) / pagesize;
    page->size = numpages * pagesize;
    if (posix_memalign(&page->addr, pagesize, page->size) != 0) {
      last_page = NULL;
      free(page);
      caml_failwith("unable to allocate memory");
    }
    total_alloc += page->size;
    live_alloc += page->size;
    page->offset = 0;
    page->refcnt = 1;
    mprotect(page->addr, page->size, PROT_READ | PROT_WRITE | PROT_EXEC);
    last_page = page;
  } else {
    page = last_page;
    page->refcnt += 1;
  }
  struct root *root = Data_custom_val(vt);
  root->page = page;
  int32_t offset = page->offset;
  int32_t modulo = offset % align;
  if (modulo != 0) offset += align - modulo;
  memcpy(page->addr + offset, String_val(vbuf), size);
  page->offset = offset + size;
  return (intnat)page->addr + offset;
}

CAMLprim value
ocaml_jitpsi_load_code (value vbuf, value size, value align, value vt)
{
  return Val_long(native_jitpsi_load_code(vbuf, Long_val(size),
					  Long_val(align), vt));
}


CAMLprim void
native_jitpsi_load_frame (value vbuf, intnat size, intnat align, value vt)
{
  struct root *root = Data_custom_val(vt);
  if (posix_memalign(&root->frametable, align, size) != 0) {
    caml_failwith("unable to allocate memory");
  }
  memcpy(root->frametable, String_val(vbuf), size);
  caml_register_frametable(root->frametable);
#ifdef FIX_4_14_0
  if (pending_frametable != NULL) {
    caml_unregister_frametable(pending_frametable);
    free(pending_frametable);
    pending_frametable = NULL;
  }
  last_frametable = root->frametable;
#endif
  return;
}

CAMLprim void
ocaml_jitpsi_load_frame (value vbuf, value size, value align, value vt)
{
  native_jitpsi_load_frame(vbuf, Long_val(size), Long_val(align), vt);
}

CAMLprim value
ocaml_jitpsi_global_symbol (value vsym)
{
  return caml_copy_int64((int64_t)caml_globalsym(String_val(vsym)));
}
