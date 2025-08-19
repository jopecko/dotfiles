# List of packages to manage with stow
PACKAGES ?= $(filter-out .git .github, $(wildcard */))

# Directory where stow will look for packages. Default is current directory
DIR ?= $$(pwd)

# Default location where stow will create symbolic links
TARGET ?= ${HOME}

IGNORE ?= \.DS_Store

# Stow command to create links
STOW_CMD = stow \
	--dir="${DIR}" \
	--target="${TARGET}" \
	--ignore="${IGNORE}" \
	--ignore="\.DS_Store" \
	--ignore=".*\.template" \
	--no-folding \
	--verbose

# Function to backup existing files for a specific package if they exist
define backup_if_exists
	checks=$$(${STOW_CMD} --no --verbose ${1} 2>&1 | \
		egrep '\* existing target is ' | \
		sed 's/  \* existing target is neither a link nor a directory: //'); \
	for file in $$checks; do \
		filepath=${TARGET}/$$file; \
		backup_suffix="backup-$$(date -u +%Y%m%d%H%M%S)"; \
		echo "Creating backup $$filepath.$$backup_suffix"; \
		mv -h "$$filepath" "$$filepath.$$backup_suffix"; \
	done
endef

# Default rule to create symbolic links for all packages
all: stow

# Rule to backup existing configurations
backup:
	@echo "Checking for existing files to backup..."
	@$(foreach package,$(PACKAGES), \
		$(call backup_if_exists,$(package));)

# Rule to link configurations using stow
stow: backup
	@echo "Applying stow for packages..."
	@$(foreach package,${PACKAGES}, \
		$(STOW_CMD) ${package};)

# Rule to remove symbolic links
unstow:
	@echo "Removing stow links for packages..."
	@$(foreach package,$(PACKAGES), \
		$(STOW_CMD) -D $(package);)

# Rule to reapply symbolic links
restow: backup unstow stow

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
	@echo "  help    - Show this help message"
	@echo ""

.PHONY: all backup stow unstow restow help

