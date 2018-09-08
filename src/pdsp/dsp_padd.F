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
      Subroutine DSP_PADD(BRUSE,RSX,RSY,RSDY,KD,KP,PD,
     +                       ISX,ISY,IWPX,IWPY,XTITLE,XDIR)
*
* Purpose :  display padd results using g2 graphics;
*             all size and position values in pixels
* Date    :  February 27, 2014
* Person  :  Wolfgang Sturhahn
*
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      Parameter(PIM = 100)
*
      Character*(*)  XTITLE,XDIR
      Character*100  XLABEL,XFORM
*
      Dimension  RSX(*),RSY(*),RSDY(*),RH(PD),RF1(4),RLOGO(PIM**2)
*
      Data ISEQ/0/
*
      Save RBLACK,RWHITE,RRED,RGREEN,RBLUE,RYELLO,RGRAY,RGRAY1,RX11,
     +     RXSIZE,RYSIZE,RXOFF,RYOFF,RYMART,RYMARB,RXMARL,RXMARR,
     +     RF1,RLX,RLY,RLOGO
*
      External LENX
*
*
      BNEW = .not.BRUSE .or. (BRUSE .and. ISEQ.eq.0)
      If(BNEW) Then
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
       RYMART = 30.
       RYMARB = 40.
*
       RXMARL = 50.
       RXMARR = 10.
*
*:  X11 window content dimension
*
       RF1(1) = RXMARL
       RF1(2) = RYMARB
       RF1(3) = RXSIZE - RXMARR
       RF1(4) = RYSIZE - RYMART
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
       RGREEN = G2_INK(RX11,0.0,0.7,0.0)
       RBLUE  = G2_INK(RX11,0.0,0.7,1.0)
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
      Else
*
*:  clear previously displayed data
*
       Call G2_CLEAR(RX11)
      Endif
*
*:  create logo
*
      RX1 = 0.0
      RY1 = RYSIZE - RLY
      Call G2_IMAGE(RX11,RX1,RY1,RLX,RLY,RLOGO)
*
*:  create description
*
      RX = RLX + 5.0
      RY = RYSIZE - 18.0
      XLABEL = 'shifted & added data'
      Call DSP_TEXT(RX11,RGRAY,18.0,0,RX,RY,XLABEL)
*
*
*:  find minimum/maximum y-values
*
      RXMIN = RSX(1)
      RXMAX = RSX(KD)
      RXR   = RXMAX - RXMIN
      RXMIN = RXMIN - 0.01*RXR
      RXMAX = RXMAX + 0.01*RXR
*
      RYMIN = RSY(KP) - 0.5*RSDY(KP)
      RYMAX = RSY(KP) + 0.5*RSDY(KP)
      Do  K = KP+1,KD
       RYMIN = Amin1(RYMIN,RSY(K)-0.5*RSDY(K))
       RYMAX = Amax1(RYMAX,RSY(K)+0.5*RSDY(K))
      EndDo
      RYR   = RYMAX - RYMIN
      RYMIN = RYMIN - 0.01*RYR
      RYMAX = RYMAX + 0.01*RYR
*
      Do  K = 1,KD
       RH(K) = 0.0
      EndDo
*
*******************************************************
*
*:  plot the data first to avoid painting the frames
*
*:  plot data as symbols with error bars
*
      Call DSP_PLOT_ERROR_BARS(RX11,RGRAY1,1.0,0,RF1,
     +               RSX,RSY,RH,RSDY,KD,RXMIN,RXMAX,RYMIN,RYMAX)
*
      Call DSP_PLOT(RX11,RBLUE,1.0,4,RF1,RSX,RSY,KD,PD,
     +                  RXMIN,RXMAX,RYMIN,RYMAX)
*
*:  create x-axis
*
      Call DSP_XAXIS(RX11,RWHITE,1.0,14.0,RF1,RXMIN,RXMAX,8,
     +                 1,1,3,0,'energy (meV)')
*
*:  create y-axis
*
      Call DSP_YAXIS(RX11,RWHITE,1.0,14.0,RF1,RYMIN,RYMAX,8,
     +                 1,1,3,0,'counts')
*
*:  create frame
*
      Call DSP_FRAME(RX11,RWHITE,1.0,16.0,RF1,' ')
*
*******************************************************
*
*:  display
*
      Call G2_FLUSH(RX11)
*
*
      End
