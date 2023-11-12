pure integer function shiftl_1(i,n) result(x)
   integer(1), value :: i
   integer, value :: n
   x = lshift(i,n)
end function
pure integer function shiftl_2(i,n) result(x)
   integer(2), value :: i
   integer, value :: n
   x = lshift(i,n)
end function
pure integer function shiftr_1(i,n) result(x)
   integer(1), value :: i
   integer, value :: n
   x = rshift(i,n)
end function
pure integer function shiftr_2(i,n) result(x)
   integer(2), value :: i
   integer, value :: n
   x = rshift(i,n)
end function