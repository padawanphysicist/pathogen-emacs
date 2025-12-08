(use-package ellama
  :ensure t
  :bind ("C-c e" . ellama)
  ;; send last message in chat buffer with C-c C-c
  :hook (org-ctrl-c-ctrl-c-final . ellama-chat-send-last-message)
  :init
  (setopt ellama-language pathogen-llm-ollama/language)
  (setopt ellama-auto-scroll t)
  (require 'llm-ollama)
  ;; (setopt ellama-provider
  ;; 	  (make-llm-ollama
  ;; 	   ;; this model should be pulled to use it
  ;; 	   ;; value should be the same as you print in terminal during pull
  ;; 	   ;;:chat-model "deepseek-r1:latest"
  ;;      :chat-model pathogen-llm-ollama/chat-model
  ;; 	   :embedding-model pathogen-llm-ollama/embedding-model))
  :config
  ;; show ellama context in header line in all buffers
  (ellama-context-header-line-global-mode +1)
  ;; show ellama session id in header line in all buffers
  (ellama-session-header-line-global-mode +1))

(elpaca-wait)

