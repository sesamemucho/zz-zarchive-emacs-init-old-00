.PHONY: clean zip elpalist gitzip

clean:
	find lisp etc -name "*.elc" -exec rm {} \;

elpalist:
	find elpa -maxdepth 1 -type d | grep -v '\(elpa\|gnupg\|archives\)$$' | sed -e 's|^elpa/||' >elpa.list

gitzip:
	zip -r ../dot-emacs-git.zip .git

zip: elpalist
#	zip -r ../dot-emacs.zip . -x \*elc
	zip -r ../dot-emacs.zip .

