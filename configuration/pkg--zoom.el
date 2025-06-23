(defun zoom/apply (n)
  (interactive "nFace-Height (in px): ")
  (set-face-attribute 'default (selected-frame) :height n))

(defun zoom/default ()
  (interactive)
  (zoom/apply 145)

  ;; (cond ((equal (display-pixel-height) 2520)
  ;;        (zoom/apply 180))

  ;;       ((equal (display-pixel-height) 1080)
  ;;        (zoom/apply 145))

  ;;       (t (zoom/apply 160)))
  )

(defun zoom/custom-1 ()
  (interactive)
  (zoom/default)
  (zoom/modify 2))

(defun zoom/custom-2 ()
  (interactive)
  (zoom/default)
  (zoom/modify 3))

(defun zoom/custom-3 ()
  (interactive)
  (zoom/default)
  (zoom/modify 4))

(defun zoom/modify (n)
  (let ((factor 10))
    (zoom/apply
     (+ (face-attribute 'default :height)
        (round (* n factor))))))

(defun zoom/inc ()
  (interactive)
  (zoom/modify 2))

(defun zoom/dec ()
  (interactive)
  (zoom/modify -2))

(zoom/default)

(provide 'pkg--zoom)
