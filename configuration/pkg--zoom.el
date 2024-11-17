(defun zoom/apply (n)
  (interactive "nFace-Height (in px): ")
  (set-face-attribute 'default (selected-frame) :height n))

(defun zoom/default ()
  (interactive)
  (cond ((equal (display-pixel-height) 2520)
         (zoom/apply 180))

        ((equal (display-pixel-height) 1080)
         (zoom/apply 190))

        (t (zoom/apply 200))))

(defun zoom/custom-1 ()
  (interactive)
  (zoom/apply 220))

(defun zoom/custom-2 ()
  (interactive)
  (zoom/apply 250))

(defun zoom/custom-3 ()
  (interactive)
  (zoom/apply 270))

(defun zoom/modify (n)
  (let ((factor 10))
    (zoom/apply
     (+ (face-attribute 'default :height)
        (* n factor)))))

(defun zoom/inc ()
  (interactive)
  (zoom/modify 2))

(defun zoom/dec ()
  (interactive)
  (zoom/modify -2))

(zoom/default)

(provide 'pkg--zoom)
