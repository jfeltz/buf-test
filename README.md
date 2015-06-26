buf-test
========
buf-test facilitates:

  * switching between subject and test source files, irregardless of programming language
  * running of a unit-test associated with current open test file buffer, or associated subject 
  * generation of unit-test source from subject file, or vice versa 
  * an architecture based on isomorphisms, thus, is extremely extensible 

example configuration:
```lisp
(require 'buf-test)
(require 's)
(require 'f)
(eval-when-compile (require 'cl))

(defun classpath-list (excluded abspath)
  "e.g: src/a/b/C.java -> [a, b, C], or src/a/b -> [a, b] "
  (f-split (f-no-ext (f-relative abspath excluded))))

(defun to-package-path (excluded abspath)
  "e.g: src/a/b/C.java -> [a, b]"
  (s-join  "." (classpath-list excluded (f-dirname abspath))))

(defun to-classpath (excluded abspath)
  (s-join  "." (classpath-list excluded abspath)))

(defun to-classname (excluded abspath)
  (car (last (classpath-list excluded abspath))))

(defun package-decl (excluded source-path)
  (concat "package " (to-package-path excluded source-path) ";")
  )

(defun source-str (excluded-source excluded-test source-path test-path)
  (assert (and (f-directory? excluded-source) (f-directory? excluded-test)))
  (concat
    (package-decl excluded-source source-path)                         "\n\n"
    "public class " (to-classname excluded-source source-path)  " {"   "\n"
    "  }"                                                              "\n"
    ))

(defun test-str (excluded-test excluded-source test-path source-path)
  (assert (and (f-directory? excluded-source) (f-directory? excluded-test)))
  (concat
    (package-decl excluded-source source-path)                       "\n\n" 
    "import static org.junit.Assert.*;"                              "\n" 
    "import org.junit.*;"                                            "\n\n"

    "/* test subject */"                                             "\n"
    "import "       (to-classpath excluded-source source-path) ";"   "\n\n"
    "public class " (to-classname excluded-test test-path)  " {"     "\n"
                                                                     "\n"
    "@Test public void test() {"                                     "\n"
    "    fail("");"                                                  "\n"
    "  }"                                                            "\n"
    "}"))

(defun make-writer (shared-root left-parent right-parent writer) 
  (assert (f-directory? shared-root) nil)
  (apply-partially
    writer (f-join shared-root left-parent) (f-join shared-root right-parent)))

(shell-command "gradle --daemon")
(setq project-root default-directory)

(defun gradle-test (test-path) 
  (concat "cd " project-root "; gradle --daemon test --tests "
    (to-classpath (concat project-root "test") test-path)))

(setq buf-test:iso (suffix-iso "Test" project-root "test" "src"))
(setq buf-test:test-runner 'gradle-test)
(setq buf-test:test-writer   (make-writer project-root "test" "src" 'test-str))
(setq buf-test:source-writer (make-writer project-root "src" "test" 'source-str))
```
