# Buffer to PDF for GNU Emacs

Create a PDF out of your current buffer, exactly as you see it. This
feature is only available to Emacs builds that have support for Cairo,
per the function `x-export-frames`.

To produce a document, go to a buffer and invoke the command
`buffer-to-pdf`. It will prompt you for an orientation among
`buffer-to-pdf-orientations`. Then it will generate the document
relative to the `buffer-to-pdf-directory`.

Advanced users can modify the `buffer-to-pdf-local-variables`,
`buffer-to-pdf-common-frame-parameters`. I am not exposing them as
user options because I think it is too early to do so.

The documentation of `buffer-to-pdf` describes the available export
methods in their order of precedence.

I learnt about the existence of `x-export-frames` from Amin Bandali,
to whom I am thankful: <https://kelar.org/~bandali/>.

## Sample configuration

```elisp
(use-package buffer-to-pdf
  :ensure nil
  :init
  ;; Then upgrade it with the command `package-vc-upgrade' or `package-vc-upgrade-all'.
  (unless (package-installed-p 'buffer-to-pdf)
    (package-vc-install "https://github.com/protesilaos/buffer-to-pdf.git"))
  :config
  ;; Configure `buffer-to-pdf-directory' to specify where PDF files are stored.
  ;; This is the default value:
  (setq buffer-to-pdf-directory (expand-file-name "~/")))
```

## Sources

- Git repository: <https://github.com/protesilaos/buffer-to-pdf>.
- Video demonstration: <https://protesilaos.com/codelog/2026-05-02-emacs-buffer-to-pdf-new-package/>.
- Backronym: Bewitched Users Find Files Effortlessly Rendered To PDF.
