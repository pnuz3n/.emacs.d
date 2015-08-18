(defun erl-exists () "Tests wether go is installed or not" 
  (= (call-process "which" nil nil nil "erl") 0)
)


(defun erl-setup () "Install erlang environment with el-get"
       (el-get-bundle erlang-mode)
)

(if (erl-exists) (erl-setup))
