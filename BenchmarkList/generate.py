import random


nums = list(range(1, 20000))

random.shuffle(nums)

with open('BenchmarkList/BenchmarkList.ml', 'w+') as file:
    file.write('let xs = [')
    for num in nums:
        file.write(f'{num};\n',)
    file.write(']')

