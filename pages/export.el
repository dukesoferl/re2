;; Copyright (c) 2017-2018 Tuncer Ayaz
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(require 'org)
(require 'ox)
(require 'ox-publish)
;; Override user settings, just in case it's used somewhere.
;; Email address is intentionally invalid.
(setq user-full-name "re2erl")
(setq user-mail-address "re2erl@re2erl.re2erl")

;; We let org-mode include its default CSS, but have to
;; make minor ergonomic adjustments.
(setf custom-html-head-extra
      (concat
        "<style type=\"text/css\">\n"
        "html {font-family: serif;}\n"
        "body {margin: auto; max-width: 45em; background: #b3b9af;}\n"
        "pre {border: 1px solid #555555; box-shadow: none;}\n"
        "</style>"))

;; Define org site (org files to export and web resources to copy)
(setq org-publish-project-alist
  `(("orgs"
     :base-directory "src"
     :base-extension "org"
     ;; Enable sitemap if needed
     ;;:auto-sitemap t
     ;;:sitemap-filename "index.org"
     ;;:sitemap-title "re2erl"
     :recursive t
     :publishing-directory "out"
     :publishing-function org-html-publish-to-html
     :with-author nil
     :with-email nil
     :with-toc nil
     :auto-preamble nil
     :section-numbers nil
     :use-sub-superscripts nil
     :html-validation-link nil
     :html-inline-images t
     ;;:html-head-include-default-style nil
     :html-head-include-scripts nil
     ;;:html-head ,custom-html-head
     :html-head-extra ,custom-html-head-extra
     :html-home/up-format ""
     :html-postamble nil)
    ;; We don't have external resources right now, but if we do,
    ;; it can be used by enabling the res component. Unused for now.
    ;;("res"
    ;; :base-directory "res"
    ;; :base-extension "css\\|png"
    ;; :publishing-directory "out"
    ;; :recursive t
    ;; :publishing-function org-publish-attachment)
    ;;("pages" :components ("orgs" "res"))))
    ("pages" :components ("orgs"))))
