local my_class = {
    value = 0,

    __add = function (self, other)
    	return self.value + other.value
    end,

    __sub = function (self, other)
    	return self.value - other.value
    end,

    __mul = function (self, other)
    	return self.value * other.value
    end,

    __div = function (self, other)
    	return self.value / other.value
    end,

    __mod = function (self, other)
    	return self.value % other.value
    end,

    __pow = function (self, other)
    	return self.value ^ other.value
    end,

    __band = function (self, other)
    	return self.value & other.value
    end,

    __bor = function (self, other)
    	return self.value | other.value
    end,

    __bxor = function (self, other)
    	return self.value ~ other.value
    end,

    __shl = function (self, other)
    	return self.value << other.value
    end,

    __shr = function (self, other)
    	return self.value >> other.value
    end,
}

-- 0101
local x = { value = 5 }
-- 1010
local y = { value = 10 }
setmetatable(x, my_class)
setmetatable(y, my_class)

-- = 15
print("+ => ", x + y)
-- = -5
print("- => ", x - y)
-- = 50
print("* => ", x * y)
-- = 0.5
print("/ => ", x / y)
-- = 9765625
print("^ => ", x ^ y)
-- = 15
print("| => ", x | y)

local a = { value = 16 }
local b = { value = 5 }
setmetatable(a, my_class)
setmetatable(b, my_class)

-- = 1
print("% => ", a % b)

-- 0110
local j = { value = 6 }
-- 0011
local k = { value = 3 }
local l = { value = 2 }
setmetatable(j, my_class)
setmetatable(k, my_class)
-- = 2
print("& => ", j & k)
-- = 0101 = 5
print("~ => ", j ~ k)
print("<< => ", j << k)
print(">> => ", b >> l)