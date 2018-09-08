#
##############################################################
#
#  PHOENIX Version xphoenix
#  Copyright (C) 2016 Wolfgang Sturhahn
#
#  This file is part of PHOENIX-xphoenix.
#
#  PHOENIX-xphoenix is free software:
#  You can redistribute it and/or modify it under the terms
#  of the GNU General Public License as published by the
#  Free Software Foundation.
#
#  PHOENIX-xphoenix is distributed in the hope that it will be
#  useful, but WITHOUT ANY WARRANTY; without even the implied
#  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#  PURPOSE. See the GNU General Public License for details.
#
#  A copy of the GNU General Public License should have been
#  provided along with PHOENIX-xphoenix.
#  If not, see <http://www.gnu.org/licenses/>.
#
#
#  purpose :  create and install PHOENIX programs
#  author  :  configure script
#  created :  Fri Sep  7 22:23:23 EDT 2018
#
##############################################################
#

include config/CONFIG

LIB_DIRS  = src/Share
EXEC_DIRS = src/padd src/phox src/psth src/psvl
TO_BE_CLEANED = $(addsuffix .x, $(LIB_DIRS) $(EXEC_DIRS))
TO_BE_INSTALLED  = padd phox psth psvl
TO_BE_INSTALLED2 = doc etc examples plugins
TO_BE_INSTALLED3 = mpadd
TO_BE_UNINSTALLED  = $(addprefix $(INSTALL_DIR)/bin/, $(TO_BE_INSTALLED))
TO_BE_UNINSTALLED2 = $(SHARE_DIR)
TO_BE_UNINSTALLED3 = $(addprefix $(INSTALL_DIR)/bin/, $(TO_BE_INSTALLED3))


.PHONY: all clean install uninstall \
	$(LIB_DIRS) $(EXEC_DIRS) $(TO_BE_INSTALLED) $(TO_BE_UNINSTALLED) \
	$(TO_BE_INSTALLED2) $(TO_BE_UNINSTALLED2)    $(TO_BE_INSTALLED3) \
	$(TO_BE_UNINSTALLED3)


all: $(LIB_DIRS) $(EXEC_DIRS)

$(LIB_DIRS):
	$(MAKE) -C $@ -f Makefile lib

$(EXEC_DIRS): $(LIB_DIRS)
	$(MAKE) -C $@ -f Makefile


clean: $(TO_BE_CLEANED)

$(TO_BE_CLEANED):
	$(MAKE) -C $(basename $@) -f Makefile clean


install: $(TO_BE_INSTALLED) $(TO_BE_INSTALLED2) $(TO_BE_INSTALLED3)

$(TO_BE_INSTALLED):
	@echo "Installing $@ ..."
	$(MKDIR) -m 755 $(INSTALL_DIR)/bin
	$(RM) $(INSTALL_DIR)/bin/$@-$(VERSION) $(INSTALL_DIR)/bin/$@
	$(CP) $(EXECTBL_DIR)/$@-$(VERSION) $(EXECTBL_DIR)/$@ $(INSTALL_DIR)/bin
	$(CHMOD) 755 $(INSTALL_DIR)/bin/$@

$(TO_BE_INSTALLED2):
	@echo "Installing $@ ..."
	$(MKDIR) -m 755 $(SHARE_DIR)
	$(RM) -r $(SHARE_DIR)/$@
	$(CP) ./$@ $(SHARE_DIR)
	$(CHMOD) 755 $(SHARE_DIR)/$@
	@if [ "$@" == plugins ]; then $(CHMOD) 755 $(SHARE_DIR)/$@/* ; fi

$(TO_BE_INSTALLED3):
	@echo "Installing $@ ..."
	$(RM) $(INSTALL_DIR)/bin/$@
	$(CP) $(EXECTBL_DIR)/$@ $(INSTALL_DIR)/bin
	$(CHMOD) 755 $(INSTALL_DIR)/bin/$@


uninstall: $(TO_BE_UNINSTALLED) $(TO_BE_UNINSTALLED2) $(TO_BE_UNINSTALLED3)

$(TO_BE_UNINSTALLED):
	@echo "Uninstalling $@ ..."
	$(RM) $@-$(VERSION) $@

$(TO_BE_UNINSTALLED2):
	@echo "Uninstalling $@ ..."
	$(RM) -r $@

$(TO_BE_UNINSTALLED3):
	@echo "Uninstalling $@ ..."
	$(RM) $@

