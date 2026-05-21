.PHONY: test check document install website help

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-15s\033[0m %s\n", $$1, $$2}'

test: ## Run tinytest::test_all()
	Rscript -e "pkgload::load_all('.'); tinytest::test_all('.')"

check: ## Full R CMD check (no manual)
	R CMD build . && R CMD check --no-manual *.tar.gz

document: ## Regenerate man pages
	Rscript -e "devtools::document('.')"

install: ## Install package locally
	R CMD INSTALL .

website: ## Build docs website (altdoc)
	Rscript -e "altdoc::render_docs(freeze = TRUE)"
