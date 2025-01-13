program Fixed_Point_iteration 
    implicit none 
    real :: x0,x1,epsilon
    integer :: iter_max,i 

    print*,"Enter initial guess ,tolerance , max iteration:"
    read*,x0,epsilon,iter_max

    i = 0 
    x1 = g(x0)

    do while(i<iter_max .and. abs(x1-x0)>epsilon)
        x0 = x1 
        x1 = g(x0)
        i = i+ 1 
    end do 

    print*,"Approximate root at x = " ,x1 

    contains 
    function g(x) result(ans)
        implicit none 
        real,intent(in)::x 
        real :: ans 

        ans = sqrt(3.0)
    end function g 
end program Fixed_Point_iteration