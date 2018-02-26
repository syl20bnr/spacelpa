# ----------------------------------------------------------------------------
#                    S P A C E L P A      M A K E F I L E
# ----------------------------------------------------------------------------

MAKEFILE_PATH = $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

BUILD_PATH        = $(MAKEFILE_PATH)/build
EMACS_CONFIG_DIR  = .emacs.d
EMACS_CONFIG_PATH = $(BUILD_PATH)/$(EMACS_CONFIG_DIR)
EMACS_SCRIPT_FILE = $(SCRIPT_DIR)/spacelpa.el
LOAD_FILES        = core/core-versions.el core/core-load-paths.el
SCRIPT_DIR        = scripts

.PHONY: clean
clean:
	rm -rf $(EMACS_CONFIG_PATH)

$(EMACS_CONFIG_PATH):
	git clone "git@github.com:syl20bnr/spacemacs" $(EMACS_CONFIG_PATH)
	cd $(EMACS_CONFIG_PATH) && git checkout develop

.PHONY: update
update: $(EMACS_CONFIG_PATH)
	HOME=$(BUILD_PATH) emacs -batch \
	                         $(addprefix -l $(EMACS_CONFIG_PATH)/, $(LOAD_FILES)) \
	                         -l $(MAKEFILE_PATH)/$(EMACS_SCRIPT_FILE)
