function add_points(p1,p2)
    local result={}
    result.x=p1.x+p2.x
    result.y=p1.y+p2.y
    return result
end --function
--
local a={}
a.x=5
a.y=6
--
local b={}
b.x=4
b.y=3
--
setmetatable(a,{__add=add_points})

local c = a + b
--
print(c.x, c.y)
return 2