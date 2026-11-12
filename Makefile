## dotfiles
# Every top-level directory is a stow package, except bin.
PACKAGES ?= $(filter-out bin,$(patsubst %/,%,$(wildcard */)))

# Directory where stow will look for packages. Default is current directory
DIR ?= $(CURDIR)

# Default location where stow will create symbolic links
TARGET ?= $(HOME)

STOW_CMD = stow \
	--dir="$(DIR)" \
	--target="$(TARGET)" \
	--ignore='\.DS_Store' \
	--verbose

# Moves aside each real file that stow reports as a conflict.
# Stow 2.4 says "over existing target X since ...", older versions say
# "existing target is neither a link nor a directory: X".
define backup_if_exists
	conflicts=$$($(STOW_CMD) --no $(1) 2>&1 | sed -n \
		-e 's/.* over existing target \(.*\) since neither a link nor a directory.*/\1/p' \
		-e 's/.*existing target is neither a link nor a directory: //p'); \
	for file in $$conflicts; do \
		filepath="$(TARGET)/$$file"; \
		backup="$$filepath.backup-$$(date -u +%Y%m%d%H%M%S)"; \
		echo "Creating backup $$backup"; \
		mv "$$filepath" "$$backup"; \
	done
endef

# Default rule to create symbolic links for all packages
all: stow

# Fetch the submodules that some packages link to (oh-my-tmux)
submodules:
	@git submodule update --init

# Rule to backup existing configurations
backup:
	@echo "Checking for existing files to backup..."
	@$(foreach package,$(PACKAGES), \
		$(call backup_if_exists,$(package));)

# Rule to link configurations using stow
stow: submodules backup
	@echo "Applying stow for packages..."
	@$(STOW_CMD) $(PACKAGES)

# Rule to remove symbolic links
unstow:
	@echo "Removing stow links for packages..."
	@$(STOW_CMD) -D $(PACKAGES)

# Rule to reapply symbolic links
restow: submodules backup
	@$(STOW_CMD) -R $(PACKAGES)

# Install the packages in Brewfile and ~/.Brewfile.local
brew:
	@brew bundle --file="$(DIR)/Brewfile"

# Run shellcheck before every commit
hooks:
	@git config core.hooksPath .githooks
	@echo "Pre-commit hook enabled"

test:
	@bin/test.sh

# Rule to display help
help:
	@echo ""
	@echo "\033[1mUSAGE\033[0m"
	@echo ""
	@echo "  make [target]"
	@echo ""
	@echo "\033[1mTARGETS\033[0m"
	@echo ""
	@echo "  stow    - Create symlinks for all packages (default)"
	@echo "  restow  - Reapply symlinks for all packages"
	@echo "  unstow  - Remove symlinks for all packages (\033[31mcaution\033[0m)"
	@echo "  brew    - Install the Homebrew packages in Brewfile"
	@echo "  hooks   - Run shellcheck before every commit"
	@echo "  test    - Run shellcheck on all shell files"
	@echo "  help    - Show this help message"
	@echo ""

.PHONY: all submodules backup stow unstow restow brew hooks test help
