      subroutine elnet  (ka,parm,no,ni,x,y,w,jd,vp,ne,nx,nlam,flmin,ulam    609
     *,thr,isd,maxit,  lmu,a0,ca,ia,nin,rsq,alm,nlp,jerr)
      real x(no,ni),y(no),w(no),vp(ni),ca(nx,nlam)                          610
      real ulam(nlam),a0(nlam),rsq(nlam),alm(nlam)                          611
      integer jd(*),ia(nx),nin(nlam)                                        612
      real, dimension (:), allocatable :: vq
