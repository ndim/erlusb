BUILD_SCRIPT_DIR = build-helpers

# Check that package version matches git version before creating dist tarballs
dist-hook: git-version-check git-version-check-news git-version-stamp
distcheck-hook: git-version-check

# Note: We cannot run autogen.sh from here, because we would need some way to
#       restart the whole dist process from the start and there is none.
EXTRA_DIST += $(top_srcdir)/$(BUILD_SCRIPT_DIR)/package-version
git-version-check:
	@git_ver=`$(top_srcdir)/$(BUILD_SCRIPT_DIR)/package-version $(top_srcdir) version-stamp`; \
	if test "x$${git_ver}" = "x$(PACKAGE_VERSION)"; then :; else \
		echo "ERROR: PACKAGE_VERSION and 'git describe' version do not match:"; \
		echo "         current 'git describe' version: $${git_ver}"; \
		echo "         current PACKAGE_VERSION:        $(PACKAGE_VERSION)"; \
		rm -rf "$(top_srcdir)/autom4te.cache"; \
		if test -f "$(top_srcdir)/autogen.sh"; then \
			echo "Update PACKAGE_VERSION by running $(top_srcdir)/autogen.sh."; \
		else \
			echo "Update PACKAGE_VERSION by running autoreconf(1)."; \
		fi; \
		exit 1; \
	fi

# FIXME: NEWS check uses ${foo%%-*} POSIX shell, tested
#        with bash, dash, busybox.
git-version-check-news:
	@git_ver=`$(top_srcdir)/$(BUILD_SCRIPT_DIR)/package-version $(top_srcdir) version-stamp`; \
	gv_xyz="$${git_ver%%-*}"; \
	gv_xy="$${gv_xyz%.*}"; \
	case `sed 1q $(top_srcdir)/NEWS` in \
	"$(PACKAGE_TARNAME) $${gv_xyz}") : ;; \
	"$(PACKAGE_TARNAME) $${gv_xy}.x") : ;; \
	"$(PACKAGE_TARNAME) $${gv_xy}.*") : ;; \
	*) \
	  echo "NEWS not updated for version $${git_ver%%-*}; not releasing" 1>&2; \
	  exit 1;; \
	esac

# Version stamp files can only exist in tarball source trees.
#
# So there is no need to generate them anywhere else or to clean them
# up anywhere.
git-version-stamp:
	echo "$(PACKAGE_VERSION)" > "$(distdir)/version-stamp"

