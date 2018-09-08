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
*  Free Software Foundation
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
      Subroutine DSP_PSTH(NCALL,BRUSE,RSX,RSY,RTY,KD,JMAX,PD,
     +                     ISX,ISY,IWPX,IWPY,XTITLE,XDIR)
*
* Purpose :  display psth results using g2 graphics
* Date    :  August 27, 2009
* Person  :  Wolfgang Sturhahn
*
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
* 
#include<implici.h>
*
      Parameter(PIM = 100, PJMAX = 10)
*
      Character*(*)  XTITLE,XDIR
      Character*100  XLABEL,XFORM
*
      Dimension  RSX(*),RSY(*),RTY(*),RSTY(PD,PJMAX),
     +           RF1(4),RF2(4),RLOGO(PIM**2)
*
      Real*4   DSP_COLOR
      External DSP_COLOR,LENX
*
      Data ISEQ/0/
*
      Save RBLACK,RWHITE,RRED,RGREEN,RBLUE,RYELLO,RGRAY,RGRAY1,RX11,
     +     RXSIZE,RYSIZE,RXOFF,RYOFF,RYMART,RYMARB,RXMARL,RXMARR,
     +     RF1,RF2,RLX,RLY,RLOGO,RXMIN,RXMAX,RYMIN,RYMAX
*
*
      If(NCALL.lt.1)  Return
*
      BNEW = .not.BRUSE .or. (BRUSE .and. ISEQ.eq.0)
      If(NCALL.eq.1 .and. BNEW) Then
*
*:  all size and position values in pixels
*
*:  window size
*
       RXSIZE = Float(ISX)
       RYSIZE = Float(ISY)
*
*:  window staggering offsets
*
       RXOFF  = -0.05*RXSIZE
       RYOFF  =  0.05*RYSIZE
*
*:  intra-window dimensions
*
       RYMART = 10.
       RYMARB = 40.
*
       RXMARL = 10.
       RXMARR = 10.
*
       RYSEP = 15.
*
*:  X11 window content dimension
*
       RF1(1) = RXMARL
       RF1(2) = RYMARB
       RF1(3) = RXSIZE - RXMARR
       RF1(4) = 0.5*RYSIZE - RYSEP
*
       RF2(1) = RXMARL
       RF2(2) = RYMARB + RYSEP + RF1(4)
       RF2(3) = RXSIZE - RXMARR
       RF2(4) = RYSIZE - RYMART
*
*:  X11 window position
*
       RXPOS  = Float(IWPX)
       RYPOS  = Float(IWPY)
       II     = Mod(ISEQ,3)
       RXPOSI = Max1(0.,RXPOS + Float(II)*RXOFF)
       RYPOSI = Max1(0.,RYPOS + Float(II)*RYOFF)
       ISEQ   = ISEQ + 1
       If(BRUSE) Then
        XLABEL = XTITLE
       Else
        IFIG   = Int(Alog10(Float(ISEQ))) + 1
        Write(XFORM,1999) IFIG
        Write(XLABEL,XFORM) XTITLE(1:LENX(XTITLE,' ')),ISEQ
       Endif
*
 1999  Format('(A,''-'',I',I1,')')
*
*:  open X11 device
*
       L = LENX(XLABEL,' ')
       RX11 = G2_OPEN_X11X(RXSIZE,RYSIZE,RXPOSI,RYPOSI,
     +                     XLABEL(1:L),XLABEL(1:L),' ',48.,48.)
*
*:  turn off autoflush to improve performance
*
       Call G2_SET_AUTO_FLUSH(RX11,0.0)
*
*:  allocate basic colors
*
       RBLACK = G2_INK(RX11,0.0,0.0,0.0)
       RWHITE = G2_INK(RX11,1.0,1.0,1.0)
       RRED   = G2_INK(RX11,1.0,0.0,0.0)
       RGREEN = G2_INK(RX11,0.0,1.0,0.0)
       RBLUE  = G2_INK(RX11,0.0,0.6,1.0)
       RYELLO = G2_INK(RX11,1.0,1.0,0.0)
       RGRAY  = G2_INK(RX11,0.8,0.8,0.8)
       RGRAY1 = G2_INK(RX11,0.5,0.5,0.5)
*
*: set background color
*
       Call G2_SET_BACKGROUND(RX11,RBLACK)
*
*:  create color array for logo
*
       Call DSP_LOGO(RX11,PIM,XDIR,NLX,NLY,RLOGO)
       RLX = FLoat(NLX)
       RLY = FLoat(NLY)
      Endif
*
      If(NCALL.eq.1) Then
*
*:  clear previously displayed data
*
       Call G2_CLEAR(RX11)
*
*:  create logo
*
       RX = 0.0
       RY = RYSIZE - RLY
       Call G2_IMAGE(RX11,RX,RY,RLX,RLY,RLOGO)
*
*:  create description
*
       RX = RF2(1) + 50.0
       RY = RF2(4) - 18.0
       XLABEL = 'excitation probability'
       Call DSP_TEXT(RX11,RGRAY,18.0,0,RX,RY,XLABEL)
*
*:  find minimum/maximum y-values
*
       RXMIN = RSX(1)
       RXMAX = RSX(KD)
       RXR   = RXMAX - RXMIN
       RXMIN = RXMIN - 0.01*RXR
       RXMAX = RXMAX + 0.01*RXR
*
       RYMIN = RSY(1)
       RYMAX = RSY(1)
       Do  K = 2,KD
        RYMIN = Amin1(RYMIN,RSY(K))
        RYMAX = Amax1(RYMAX,RSY(K))
       EndDo
       RYR   = RYMAX - RYMIN
       RYMIN = RYMIN - 0.01*RYR
       RYMAX = RYMAX + 0.03*RYR
*
*******************************************************
*
*:  plot the data first to avoid painting the frames
*
*:  plot data as symbols
*
       Call DSP_PLOT(RX11,RGREEN,1.0,4,RF2,RSX,RSY,KD,PD,
     +                  RXMIN,RXMAX,RYMIN,RYMAX)
*
*:  create x-axis
*
       Call DSP_XAXIS(RX11,RWHITE,1.0,14.0,RF2,RXMIN,RXMAX,8,
     +                 1,1,1,0,'energy (meV)')
*
*:  create frame
*
       Call DSP_FRAME(RX11,RWHITE,1.0,16.0,RF2,' ')
*
*******************************************************
*
      Endif
*
*
      If(NCALL.eq.2) Then
*
*:  find minimum/maximum y-values
*
       RXMIN = RSX(1)
       RXMAX = RSX(KD)
       RXR   = RXMAX - RXMIN
       RXMIN = RXMIN - 0.01*RXR
       RXMAX = RXMAX + 0.01*RXR
*
       RYMIN = RTY(1)
       RYMAX = RTY(1)
       Do  K = 2,KD
        RYMIN = Amin1(RYMIN,RTY(K))
        RYMAX = Amax1(RYMAX,RTY(K))
       EndDo
*
*:  save data
*
       Do  K = 1,KD
        RSTY(K,1) = RTY(K)
       EndDo
      Endif
*
      If(NCALL.gt.2 .and. NCALL.le.PJMAX+1) Then
*
*:  update minimum/maximum y-values
*
       Do  K = 1,KD
        RYMIN = Amin1(RYMIN,RTY(K))
        RYMAX = Amax1(RYMAX,RTY(K))
       EndDo
*
*:  save data
*
       Do  K = 1,KD
        RSTY(K,NCALL-1) = RTY(K)
       EndDo
      Endif
*
*
      If(NCALL.eq.JMAX+1) Then
*
*:  create description
*
       RX = RF1(1) + 7.0
       RY = RF1(4) - 19.0
       XLABEL = 'n-phonon terms (log-scale)'
       Call DSP_TEXT(RX11,RGRAY,18.0,0,RX,RY,XLABEL)
*
*******************************************************
*
*:  plot the data first to avoid painting the frames
*
*:  plot data as line
*
       RCUT  = 0.0001*RYMAX
       RYMAX = Alog10(RYMAX)
       RYMIN = Alog10(Amax1(RYMIN,RCUT))
*
       RYR   = RYMAX - RYMIN
       RYMAX = RYMAX + 0.03*RYR
*
       JM  = Min0(PJMAX,NCALL-1)
       RDT = 2700./Float(Max0(1,JM-1))
       Do  J = 1,JM
*
*:  convert to log-scale
*
        Do  K = 1,KD
         RTY(K) = Alog10(Amax1(RSTY(K,J),RCUT))
        EndDo
*
        RTEMP  = 3000. - RDT*Float(J-1)
        RCOLOR = DSP_COLOR(RX11,300.,3000.,RTEMP)
*
        Call DSP_PLOT(RX11,RCOLOR,1.0,0,RF1,RSX,RTY,KD,PD,
     +                  RXMIN,RXMAX,RYMIN,RYMAX)
       EndDo
*
*******************************************************
*
*:  create x-axis
*
       Call DSP_XAXIS(RX11,RWHITE,1.0,14.0,RF1,RXMIN,RXMAX,8,
     +                 1,1,1,0,'energy (meV)')
*
*:  create frame
*
       Call DSP_FRAME(RX11,RWHITE,1.0,16.0,RF1,' ')
*
*******************************************************
*
      Endif
*
*:  display
*
      Call G2_FLUSH(RX11)
*
*
      End
