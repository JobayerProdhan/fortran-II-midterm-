program false_position
    implicit none 
    real :: a,b,c,fa,fb,fc,epsilon
    integer :: i, max_iter
    logical :: success

    print*,"Enter [a,b],max iteration and tolerance:"
    read*,a,b,max_iter,epsilon

    fa = f(a)
    fb  = f(b) 

    if(fa*fb>0.0)then
        print*,"Root doesn't exit"
        stop
    end if 

    i = 0 
    success = .false.

    do while (i<max_iter .and. abs(b-a)>epsilon)
        c = b-(f(b)*(a-b))/(f(a)-f(b))
        fc = f(c)

        if(abs(fc)<=epsilon)then 
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
        print*,"root is:",c 
    end if 

    

    contains
    function f(x) result(ans)
        real,intent(in) :: x
        real :: ans 

        ans = x**3 - x - 2.0
    end function f 
end program false_position