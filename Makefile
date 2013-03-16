
dir  = $(HOME)/.zsh/git-prompt
src += gitstatus.py
src += zshrc.sh
dst  = $(addprefix $(dir)/, $(src))

.PHONY: install

install: $(dir) $(dst)

$(dir):
	mkdir -vp $(dir)

$(dir)/%: %
	sudo install -m 0444 $< $@

