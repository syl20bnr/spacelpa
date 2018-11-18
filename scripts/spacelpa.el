;;; spacelpa.el
;;
;; Copyright (c) 2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacelpa
;;
;; This file is not part of GNU Emacs.
;;
;; Creates/update the spacelpa ELPA repository.
;;
;;; License: GPLv3

(require 'core-spacemacs)

(configuration-layer/load-lock-file)
(spacemacs/init)
(configuration-layer/load)

(let ((dir (expand-file-name
            (concat (file-name-directory load-file-name)
                    "../packages" ))))
  (configuration-layer/create-elpa-repository "" dir))
