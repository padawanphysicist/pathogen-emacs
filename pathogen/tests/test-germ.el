;; Como executar o teste interativemente: com este buffer aberto,
;;   1. Execute `M-x eval-buffer`,
;;   2. Execute `M-x ert`. Quando for questionado para escolher um teste,
;;   digite `t' (padrão) para executar todos os testes ou escolha um teste
;;   específico (test-pathogen-germ-instantiation).
;; Um buffer será exibido com os resultados do teste (onde . significa teste
;; bem sucedido, E indica um erro e F uma falha).
;;
;; https://www.gnu.org/software/emacs/manual/html_node/ert/Running-Tests-Interactively.html

(add-to-list 'load-path (expand-file-name "pathogen/core"))
(add-to-list 'load-path (expand-file-name "pathogen/utils"))

(require 'ert)
(require 'pathogen-germ)

;; Verifica a instanciação da classe definida em
;; ~/.emacs.d/pathogen/core/pathogen-germ.el
(ert-deftest test-pathogen-germ-instantiation-default ()
  "Verify that creating a germ initialize the object properly."
  (let ((pathogen-germs-directory "/tmp/pathogen/"))
    (let ((test-germ (pathogen-germ :name '+test/hello)))
      (should (equal (pathogen-germ-name test-germ) '+test/hello))
      (should (equal (pathogen-germ-dependencies test-germ) nil))
      (should (equal (pathogen-germ-path test-germ) ""))
      (should (equal (pathogen-germ-variables test-germ) nil))
      (should (equal (pathogen-germ-enabled-p test-germ) t))
      (should (equal (pathogen-germ-loaded-p test-germ) nil)))))

(ert-deftest test-pathogen-germ-instantiation-with-dependencies ()
  "Verify that creating a germ with dependencies initialize the object properly."
  (let ((pathogen-germs-directory "/tmp/pathogen/"))
    (let ((test-germ (pathogen-germ :name '+test/world :dependencies '(+test/hello))))
      (should (equal (pathogen-germ-name test-germ) '+test/world))
      (should (equal (pathogen-germ-dependencies test-germ) '(+test/hello)))
      (should (equal (pathogen-germ-path test-germ) ""))
      (should (equal (pathogen-germ-variables test-germ) nil))
      (should (equal (pathogen-germ-enabled-p test-germ) t))
      (should (equal (pathogen-germ-loaded-p test-germ) nil)))))

(ert-deftest test-pathogen-germ-discover-path ()
  "Verify that creating a germ initialize the object properly."
  (let ((pathogen-germs-directory "/tmp/pathogen/"))
    (let ((test-germ (pathogen-germ :name '+test/hello)))
      (pathogen-germ--set-path! test-germ)
      (should (equal (pathogen-germ-name test-germ) '+test/hello))
      (should (equal (pathogen-germ-dependencies test-germ) nil))
      (should (equal (pathogen-germ-path test-germ) "/tmp/pathogen/+test/hello"))
      (should (equal (pathogen-germ-variables test-germ) nil))
      (should (equal (pathogen-germ-enabled-p test-germ) t))
      (should (equal (pathogen-germ-loaded-p test-germ) nil)))))

;(ert-deftest test-pathogen-germ-registration ()
;  "Verify that creating a germ triggers registration in the genome."
;  ;; 1. Setup: Mock the global genome and the germs directory
;  (let ((*pathogen-genome* (make-hash-table :test 'eq))
;        (pathogen-germs-directory "/tmp/pathogen/"))
;    
;    (let ((test-germ (pathogen-germ :name 'ui/theme 
;                                   :path "custom-path/")))
;      
;      ;; 2. Manually trigger register (cl-defmethod requires explicit call 
;      ;; unless you intended for initialize-instance to call it)
;      (register test-germ)
;
;      ;; 3. Check if registration side-effect occurred
;      (should (gethash 'ui/theme *pathogen-genome*))
;      (should (eq (gethash 'ui/theme *pathogen-genome*) test-germ))
;      
;      ;; 4. Check log-ready name and path
;      (should (equal (pathogen-germ-name test-germ) 'ui/theme))
;      (should (equal (pathogen-germ-path test-germ) "custom-path/")))))
;
;(ert-deftest test-pathogen-germ-find-path ()
;  "Test the directory resolution logic of find-path."
;  (let ((pathogen-germs-directory "/home/user/.emacs.d/germs/"))
;    (let ((test-germ (pathogen-germ :name 'editors/vim)))
;      
;      ;; Note: Your implementation of find-path uses 'name' which looks 
;      ;; like a free variable. Ensure it uses (pathogen-germ-name obj).
;      (let ((resolved-path (find-path test-germ)))
;        (should (equal resolved-path "/home/user/.emacs.d/germs/editors/vim"))))))
;
;(ert-deftest test-pathogen-germ-boolean-states ()
;  "Ensure default state for enabled-p and loaded-p is correct."
;  (let ((test-germ (pathogen-germ :name 'test)))
;    (should (pathogen-germ-enabled-p test-germ))
;    (should-not (pathogen-germ-loaded-p test-germ))))
