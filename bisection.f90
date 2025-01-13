program Bisection_Method 
    implicit  none  
    real :: a,b,c,fa,fb,fc,epsilon
    integer :: i,max_iter
    logical :: success

    print*,"Enter the [a,b] , max iteration and tolerance:"
    read*,a,b,max_iter,epsilon

    fa = f(a)
    fb = f(b)

    if(fa*fb>0.0)then 
        print*,"Root doesn't exits"
        stop
    end if 

    i = 0 
    success = .false. 

    do while(i<max_iter .and. abs(b-a)>epsilon)
        c= (a+b)/2.0
        fc = f(c)

        if(abs(fc) <=epsilon)then 
            success = .true.
            exit 
        end if

        if(fa*fc<0.0)then 
            b = c 
            fb = fc 
        else 
            a = c 
            fa = fc 
        end if 

        i = i+ 1 
    end do 

    if(success)then 
        print*,"Ans is:",c 
        print*,"Iteration is: " ,i 
    else
        print*,"Doesn't converge by this number of iteration " 
    end if 


    contains 
    function f(x) result(ans)
        real,intent(in) :: x 
        real :: ans 

        ans = x**3 - x - 2.0
    end function 
end program Bisection_Method


! program Bisection_Method
!     implicit none
!     real :: a, b, c, fa, fb, fc, epsilon
!     integer :: i, max_iter
!     logical :: success

!     ! Input: Interval, tolerance, and maximum iterations
!     print*, "Enter the [a,b], max iteration and tolerance:"
!     read*, a, b, max_iter, epsilon

!     ! Evaluate function at the endpoints
!     fa = f(a)
!     fb = f(b)

!     ! Check if the interval contains a root
!     if (fa * fb > 0.0) then
!         print*, "Root doesn't exist"
!         stop
!     end if

!     i = 0
!     success = .false.

!     ! Main loop: Continue until the interval is small enough or max iterations reached
!     do while (i < max_iter .and. abs(b - a) > epsilon)
!         c = (a + b) / 2.0
!         fc = f(c)

!         ! Check for convergence
!         if (abs(fc) <= epsilon) then
!             success = .true.
!             exit
!         end if

!         ! Update the interval based on sign change
!         if (fa * fc < 0.0) then
!             b = c
!             fb = fc
!         else
!             a = c
!             fa = fc
!         end if

!         i = i + 1
!     end do

!     ! Output the result
!     if (success) then
!         print*, "Ans is:", c
!         print*, "Iteration is:", i
!     else
!         print*, "Doesn't converge by this number of iterations"
!     end if

!     contains
!     function f(x) result(ans)
!         real, intent(in) :: x
!         real :: ans

!         ans = x**3 - x - 2.0
!     end function
! end program Bisection_Method
