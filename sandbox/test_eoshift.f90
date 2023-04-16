integer :: keys(8)
do i = 1, 8
    keys(i) = i
end do
keys = eoshift(keys,1,boundary=-5)
print *, keys
end