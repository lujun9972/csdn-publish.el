(require 'subr-x)
(require 'org)
(require 'ox-gfm)
(require 'vc)
(require 'request)
(require 'browse-url)

(defcustom csdn-publish-interval 61
  "Seconds wait between two publish."
  :group 'csdn-publish
  :type 'integer)

(defcustom csdn-publish-user-name (getenv "CSDN_PUBLISH_USER_NAME")
  "value of UserName in Cookie which used to login in CSDN"
  :group 'csdn-publish
  :type 'string)

(defcustom csdn-publish-user-info (getenv "CSDN_PUBLISH_USER_INFO")
  "value of UserInfo in Cookie which used to login in CSDN"
  :group 'csdn-publish
  :type 'string)

(defcustom csdn-publish-user-token (getenv "CSDN_PUBLISH_USER_TOKEN")
  "value of UserToken in Cookie which used to login in CSDN"
  :group 'csdn-publish
  :type 'string)

(defcustom csdn-publish-open-url 'confirm
  "Whether open article URL after publish.

nil means don't open.
'confirm means ask user.
t means open the URL."
  :group 'csdn-publish
  )

(defun csdn-publish-get-cookie ()
  "Get the cookie used to login in CSDN."
  (format "UserName=%s; UserInfo=%s; UserToken=%s" csdn-publish-user-name csdn-publish-user-info csdn-publish-user-token))

(defcustom csdn-publish-original-link-getter (if (featurep 'ego)
                                                 #'csdn-publish-get-ego-link
                                               #'csdn-publish-convert-link)
  "Function used to get original link.

This function accept the `buffer-file-name' as the only parameter"
  :group 'csdn-publish
  :type 'function)

(defun csdn-publish-get-ego-link (filename)
  (let* ((vc-root (file-name-as-directory (file-truename (vc-git-root filename))))
         (project (cl-find-if (lambda (project)
                                (let* ((properties (cdr project))
                                       (repository-directory (plist-get properties :repository-directory))
                                       (abs-path (file-name-as-directory (file-truename repository-directory))))
                                  (string= vc-root abs-path)))
                              ego-project-config-alist)))
    (if project
        (let* ((site-domain (plist-get (cdr project) :site-domain))
               (ego-current-project-name (car project))
               (options (car (ego--get-org-file-options filename vc-root nil)))
               (uri (plist-get options :uri)))
          (concat (replace-regexp-in-string "/?$" ""  site-domain) uri))
      (csdn-publish-convert-link filename))))

(defcustom csdn-publish-db-file-name
  ".csdn.db"
  "The file used to store csdn-publish related information."
  :group 'csdn-publish
  :type 'string)

(defun csdn-publish-get-db-file ()
  "Return the absolute path of db-file in blog's repo root directory"
  (let ((root-dir (vc-root-dir)))
    (expand-file-name csdn-publish-db-file-name root-dir)))

(defun csdn-publish-get-org-csdn-mapping ()
  "Return org-csdn-mapping-alist which stored in csdn-publish-db-file(use csdn-publish-get-db-file function to get it)."
  (let* ((csdn-publish-db-file (csdn-publish-get-db-file)))
    (when (file-readable-p csdn-publish-db-file)
      (with-temp-buffer
        (insert-file-contents csdn-publish-db-file)
        (read (current-buffer))))))

(defun csdn-publish-save-org-csdn-mapping (mapping-alist)
  "Save mapping-alist to csdn-publish-db-file(use csdn-publish-get-db-file function to get it)."
  (let ((csdn-publish-db-file (csdn-publish-get-db-file)))
    (with-temp-file csdn-publish-db-file
      (print mapping-alist (current-buffer)))))

(defun csdn-publish-update-org-csdn-mapping (org-path csdn-id)
  "update org-path and csdn-id mapping relationship stored in csdn-publish-db-file(use csdn-publish-get-db-file function to get it).

Return the origin csdn id of ORG-PATH."
  (let* ((org-csdn-mapping-alist (csdn-publish-get-org-csdn-mapping))
         (origin-csdn-id (assoc-default org-path org-csdn-mapping-alist)))
    (if origin-csdn-id
        (setf (cdr (assoc org-path org-csdn-mapping-alist)) csdn-id)
      (push (cons org-path csdn-id) org-csdn-mapping-alist))
    (csdn-publish-save-org-csdn-mapping org-csdn-mapping-alist)
    origin-csdn-id))

(defun csdn-publish-get-origin-csdn-id (org-path)
  "Return the origin csdn id of ORG-PATH."
  (let* ((org-csdn-mapping-alist (csdn-publish-get-org-csdn-mapping))
         (origin-csdn-id (assoc-default org-path org-csdn-mapping-alist)))
    origin-csdn-id))


(defun csdn-publish-convert-link (&optional filename)
  (interactive)
  (let* ((filename (or filename (read-file-name "Select a file")))
         (root-path (vc-git-root filename))
         ;; (asset-abs-path (expand-file-name default-directory filename))
         (relative-path (file-relative-name filename root-path))
         (remote-url
          (with-temp-buffer
            (insert (vc-git-dir-extra-headers root-path))
            (when (re-search-backward "^Remote[[:blank:]]+:[[:blank:]]+\\([^[:blank:]\r\n]+\\)" nil t)
              (match-string 1))))
         (branch
          (with-temp-buffer
            (insert (vc-git-dir-extra-headers root-path))
            (when (re-search-backward "^Branch[[:blank:]]+:[[:blank:]]+\\([^[:blank:]\r\n]+\\)" nil t)
              (match-string 1))))
         (converted-path (when (string-match "^git@github.com:\\([^/]+\\)/\\(.+\\).git" remote-url)
                           (let ((site "raw.githubusercontent.com")
                                 (user (match-string-no-properties 1 remote-url))
                                 (repo (match-string-no-properties 2 remote-url)))
                             (format "https://%s/%s/%s/%s/%s" site user repo branch relative-path)))))
    (when (interactive-p)
      (message "%s" converted-path)
      (kill-new converted-path))
    converted-path))

(defcustom csdn-publish-convert-rules
  '(("<[a-zA-Z]+[^/>]+\\(src\\|href\\)=\"\\([^#\"][^\"]*\\)\"[^>]*>" . 2)) ;HTML link
  "Alist of filename REGEXP vs NUM.
Each element looks like (REGEXP . NUM).
NUM specifies which parenthesized expression in the regexp should be replaced.")

(defun csdn-publish-convert-body (body &optional filename)
  (let ((filename (or filename (buffer-file-name))))
    (with-temp-buffer
      (insert body)
      (save-excursion
        (dolist (pair csdn-publish-convert-rules)
          (goto-char (point-min))
          (let ((regex (car pair))
                (num (cdr pair)))
            (while (re-search-forward regex nil t)
              (let* ((asset-path (match-string num))
                     (asset-path-begin (match-beginning num))
                     (asset-path-end (match-end num))
                     (asset-abs-path (expand-file-name asset-path (file-name-directory filename))))
                (unless (url-type (url-generic-parse-url asset-path)) ;; 判断是否为绝对路径的URI
                  (if (not (file-exists-p asset-abs-path))
                      (message "[WARN] File %s in hyper link does not exist, org file: %s." asset-abs-path filename)
                    (let ((converted-path (csdn-publish-convert-link asset-abs-path)))
                      (when converted-path
                        (setf (buffer-substring asset-path-begin asset-path-end) converted-path))))))))))
      (buffer-string))))

(defun csdn-publish--read-org-option (option)
  "Read option value of org file opened in *current buffer*.
e.g:
#+TITLE: this is title
will return \"this is title\" if OPTION is \"TITLE\""
  (let ((match-regexp (org-make-options-regexp `(,option))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward match-regexp nil t)
        (match-string-no-properties 2 nil)))))

;;;###autoload
(defun csdn-publish (&optional read-type)
  "Publish current article"
  (interactive)
  (message "DEBUG:current buffer is [%s],now is [%s]" (buffer-name) (current-time-string))
  (let* ((md-content (org-export-as 'gfm nil nil t))
         (original_link (funcall csdn-publish-original-link-getter (buffer-file-name)))
         (origin-article-statement (format "<p>原文地址:<a href='%s'>%s</a></p>" original_link original_link))
         (html-content (concat origin-article-statement "\n" (csdn-publish-convert-body (org-export-as 'html nil nil t))))
         (read-type (or read-type "public"))
         (title (csdn-publish--read-org-option "TITLE"))
         (tags (or (csdn-publish--read-org-option "TAGS") ""))
         (category (car (split-string tags " ")))
         (data `(("title" . ,title)
                 ("markdowncontent" . ,md-content)
                 ("content" . ,html-content)
                 ("readType" . ,read-type)
                 ("tags" . ,tags)
                 ("status" . 0)
                 ("categories" . ,category)
                 ("type" . "original")
                 ("original_link" . ,original_link)
                 ("authorized_status" . nil)
                 ("not_auto_saved" . "1")
                 ("source" . "pc_mdeditor")))
         (url-request-extra-headers `(("Accept-Charset" . "zh-CN,en-US;q=0.7,en;q=0.3")
                                      ("Content-Type" . "application/json")
                                      ("Referer" . "https://editor.csdn.net/md")
                                      ("Cookie" . ,(csdn-publish-get-cookie))
                                      ("Cache-Control" . "no-cache"))))
    (request "https://blog-console-api.csdn.net/v1/mdeditor/saveArticle"
      :type "POST"
      :data (json-encode data)
      :headers url-request-extra-headers
      :parser #'json-read
      :sync t
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (message "data is %s" data)
                  (let* ((data (alist-get 'data data))
                         (id (alist-get 'id data))
                         (url (alist-get 'url data))
                         (open-url-p (if (eq csdn-publish-open-url 'confirm)
                                         (yes-or-no-p (format "Open blog url(%s)?" url))
                                       csdn-publish-open-url)))
                    (when open-url-p
                      (browse-url url))
                    (csdn-publish-update-org-csdn-mapping title id))))
      :error (cl-function
              (lambda (&key data &allow-other-keys)
                (message "get a failed responese[%s]" data)
                (let ((code (alist-get 'code data))
                      (msg (alist-get 'msg data)))
                  (warn code msg)))))))

;;;###autoload
(defun csdn-publish-articles (&optional files read-type)
  "Publish specifed files"
  (interactive)
  (let ((files (or files (read-file-name "Select a file"))))
    (when (stringp files)
      (setq files (list files)))
    (dolist (file files)
             (save-excursion
               (find-file file)
               (csdn-publish)
               (sit-for csdn-publish-interval)
               (kill-buffer)))))

(provide 'csdn-publish)
