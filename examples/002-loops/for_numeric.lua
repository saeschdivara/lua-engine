function f(x)
    print("foo", x)
	return x+1
end

local x = 10
print("For loop up")
for i = 1, f(x) do print(i) end

print("For loop down")
for i = 10, 1, -1 do print(i) end
