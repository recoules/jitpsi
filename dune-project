(lang dune 2.9)
(generate_opam_files true)

(name jitpsi)
(version "0.0.0")
(maintainers "Frédéric Recoules <frederic.recoules@cea.fr>")
(authors "Frédéric Recoules")
(source (github recoules/jitpsi))
(license GPL-3.0-or-later)
(bug_reports "https://github.com/recoules/jitpsi/issues")

(package
 (name jitpsi)
 (synopsis "Just In Time Partial Specialization for Interpreter")
 (depends
  (ocaml (and (>= 4.14) (< 5))))
 (tags
  ("JIT"
   "interpreter")))