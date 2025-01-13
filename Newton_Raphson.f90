program NewtonRaphson 
    implicit  none 
    real :: x0,x1,epsilon
    integer :: i,iter_max 

    print*,"Enter the initial guess, max iteration and tolerance:"
    read*,x0,iter_max,epsilon

    x1 = x0 - f(x0)/df(x0)
    i = 0 
    do while(i<iter_max .and. abs(x1-x0)>epsilon)
        x0 = x1 
        x1  = x0 -f(x0)/df(x0)
        i = i + 1 
    end do 

    print*,"Root is:",x1 

    contains
    function f(x) result(ans)
        implicit none 
        real,intent(in) :: x 
        real :: ans 
        ans = x**2 - 2.0 
    end function f 

    function df(x) result(ans)
        implicit  none 
        real,intent(in) :: x 
        real :: ans 
        ans = 2**x 
    end function df 
end program NewtonRaphson