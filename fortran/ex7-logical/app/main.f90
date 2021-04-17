program roots

  implicit none

  intrinsic :: selected_real_kind, abs, sqrt
  integer, parameter :: wp = selected_real_kind(15)
  real(wp) :: p, q, d

  write (*, *) "Solving x² + p·x + q = 0, please enter p and q"
  read (*, *) p, q
	d = 0.25_wp * p**2 - q
	
	! descriminant is positive, we have two real roots
  if (d > 0.0_wp) then
    write (*, *) "x1 =", -0.5_wp*p + sqrt(d)
    write (*, *) "x2 =", -0.5_wp*p - sqrt(d)
	! descriminant is negative, we have two complex roots
  else if (d < 0.0_wp) then
    write (*, *) "x1 =", -0.5_wp*p, "+ i ·", sqrt(abs(d))
    write (*, *) "x2 =", -0.5_wp*p, "- i ·", sqrt(abs(d))
	! descriminant is zero, we have only one root
	else  
    write(*, *) "x1 = x2 =", -0.5_wp * p
  end if

end program roots
