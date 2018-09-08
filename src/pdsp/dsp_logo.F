*
**************************************************************
*
*  PHOENIX Version 2.1.4
*  Copyright (C) 2016 Wolfgang Sturhahn
*
*  This file is part of PHOENIX-2.1.4.
*
*  PHOENIX-2.1.4 is free software:
*  You can redistribute it and/or modify it under the terms
*  of the GNU General Public License as published by the
*  Free Software Foundation.
*
*  PHOENIX-2.1.4 is distributed in the hope that it will be
*  useful, but WITHOUT ANY WARRANTY; without even the implied
*  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*  PURPOSE. See the GNU General Public License for details.
*
*  A copy of the GNU General Public License should have been
*  provided along with PHOENIX-2.1.4.
*  If not, see <http://www.gnu.org/licenses/>.
*
**************************************************************
*
      Subroutine DSP_LOGO(RX11,PIM,XDIR,NX,NY,RLOGO)
*
* Purpose :  create color map for logo image
* Date    :  February 26, 2014
* Author  :  Wolfgang Sturhahn
*
*
* import variables :
*    RX11     = X11 device pointer
*    PIM      = array dimension
*    XDIR     = bin directory path
*
* export variables :
*    NX       = image width
*    NY       = image height
*    RLOGO(*) = NX*NY colors representing the image
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      Character*(*) XDIR
*
      Character*20  XVERSION
      Character*200 XFILE
*
      Dimension  IMAGE(3,PIM,PIM),RLOGO(*)
*
#include<version.h>
*
*:  construct file name
*
      L   = Lenx(XDIR,' ')
      XFILE = XDIR(1:L-4)
      If(L.gt.167) Then
       NX = 0
       NY = 0
       Return
      Endif
      If(Index(XVERSION,'@1').eq.0) Then
       Call XMERGE(XFILE,'/share/PHOENIX-'//XVERSION)
      Endif
      Call XMERGE(XFILE,'/etc/NRIXSlogo.ppm')
*
      Inquire(File=XFILE,Exist=B)
      If(.not.B) Then
       NX = 0
       NY = 0
       Return
      Endif
*
*:  read ppm file
*
      Call READPPM(XFILE,PIM,NX,NY,IMAGE,NERR)
      If(NERR.ne.0) Then
       NX = 0
       NY = 0
      Else
       RFAC = 1./255.
       II   = 1
       Do  IY = NY,1,-1
        Do  IX = 1,NX
         RRED = RFAC*Float(IMAGE(1,IX,IY))
         RGRN = RFAC*Float(IMAGE(2,IX,IY))
         RBLU = RFAC*Float(IMAGE(3,IX,IY))
         RLOGO(II) = G2_INK(RX11,RRED,RGRN,RBLU)
         II = II + 1
        EndDo
       EndDo
      Endif
*
*
      End
