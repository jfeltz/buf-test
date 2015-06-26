(load-file "../buf-test.el")
(require 'buf-test)
(require 'ert)
(defun buf-test-init ()
  (setq buf-test:iso (suffix-iso "Test" default-directory "test" "src"))
  )
(defun buf-test-fixture (body)
  (unwind-protect
    (progn (buf-test-init) (funcall body)) (buf-test-init)))
(ert-deftest test-to-alt ()
  (should (eq 'source-t (to-alt 'test-t)))
  (should (eq 'test-t (to-alt 'source-t))))
(ert-deftest test-to-fn-morph ()
  (should (to-fn-morph buf-test:iso 'test-t))
  (should (to-fn-morph buf-test:iso 'source-t)))
(ert-deftest test-to-writer ()
  (should (to-writer 'test-t))
  (should (to-writer 'source-t)))
(ert-deftest test-from-iso-pred ()
  (buf-test-fixture (lambda ()
    (should
     (eq
      'test-t
       (from-iso-pred 'test-t
       (f-join default-directory "test/a/d_test.cpp") buf-test:iso)))
    (should
      (eq
        'source-t
        (from-iso-pred 'source-t
        (f-join default-directory "src/a/d.cpp") buf-test:iso))))))

;;;;;;;;;;;;;;;;; tests for isomorphisms

(ert-deftest test-rooted()
  (should (rooted "/a" "test" "/a/test/abc_sfx.cpp")))

;; tests on the suffix isomorphism
(ert-deftest test-is-test()
  (should (suffix-iso:is-test "_sfx" "/a" "test" "/a/test/abc_sfx.cpp"))
  (should (suffix-iso:is-test "_sfx" "/a" nil "/a/abc_sfx"))
  (should (not (suffix-iso:is-test "_sfx" "/a" "test" "/a/test/_sfx.cpp")))
  (should (not (suffix-iso:is-test "_sfx" "/a" "test" "/a/test/abc.cpp")))
  (should (not (suffix-iso:is-test "_sfx" "/a" "test" "/a/test/_sfx_abc.cpp"))))

(ert-deftest test-is-source()
  (should (iso:is-source "/a" "src" "/a/src/abc_sfx.cpp"))
  (should (iso:is-source "/a" nil "/a/abc_sfx"))
  (should (iso:is-source "/a" "" "/a/_sfx_abc.cpp"))
  (should (not (iso:is-source "/a" "src" "/b/src/_sfx_abc.cpp")))
  (should (not (iso:is-source "/a" "src" "/a/srd/_sfx_abc.cpp"))))

(ert-deftest test-suffix-iso:src-to-test ()
  (should
   (string-equal "/a/test/abc_sfx.cpp"
    (suffix-iso:src-to-test "_sfx" "/a" "test" "src" "/a/src/abc.cpp")))
  (should
   (string-equal "/a/test/abc.cpp"
    (suffix-iso:src-to-test "" "/a" "test" "src" "/a/src/abc.cpp")))
  (should
   (string-equal "/a/test/abc.cpp"
    (suffix-iso:src-to-test "" "/a" "test" "" "/a/abc.cpp")))
  (should
    (string-equal "/a/abc.cpp"
      (suffix-iso:src-to-test "" "/a" "" "" "/a/abc.cpp"))))
  
(ert-deftest test-suffix-iso:test-to-src ()
  (should
   (string-equal "/a/src/abc.cpp"
    (suffix-iso:test-to-src "_sfx" "/a" "test" "src" "/a/test/abc_sfx.cpp")))
  (should
   (string-equal "/a/src/abc.cpp"
    (suffix-iso:test-to-src "" "/a" "" "src" "/a/abc.cpp")))
  (should
   (string-equal "/a/abc.cpp"
    (suffix-iso:test-to-src "" "/a" "test" "" "/a/test/abc.cpp")))
  (should
    (string-equal "/a/abc.cpp"
      (suffix-iso:test-to-src "" "/a" "" "" "/a/abc.cpp"))))
