import random
import time
import tracemalloc
from typing import List, Callable, Optional, TypeVar, Generic

T = TypeVar('T')
U = TypeVar('U')

class FunctionalArray(Generic[T]):
    def __init__(self, elements: List[T]):
        self.elements = elements

    def __repr__(self):
        return f"FunctionalArray({self.elements})"

    def get(self, index: int) -> Optional[T]:
        if 0 <= index < len(self.elements):
            return self.elements[index]
        return None

    def update(self, index: int, value: T) -> 'FunctionalArray[T]':
        if 0 <= index < len(self.elements):
            new_elements = self.elements[:index] + [value] + self.elements[index + 1:]
            return FunctionalArray(new_elements)
        raise IndexError("Index out of bounds")

    def append(self, value: T) -> 'FunctionalArray[T]':
        return FunctionalArray(self.elements + [value])

    def prepend(self, value: T) -> 'FunctionalArray[T]':
        return FunctionalArray([value] + self.elements)

    def length(self) -> int:
        return len(self.elements)

    def slice(self, start: int, end: int) -> 'FunctionalArray[T]':
        return FunctionalArray(self.elements[start:end])

    def map_array(self, func: Callable[[T], U]) -> 'FunctionalArray[U]':
        return FunctionalArray(list(map(func, self.elements)))

    def filter_array(self, predicate: Callable[[T], bool]) -> 'FunctionalArray[T]':
        return FunctionalArray(list(filter(predicate, self.elements)))

    def fold_array(self, func: Callable[[U, T], U], acc: U) -> U:
        result = acc
        for element in self.elements:
            result = func(result, element)
        return result

    def insert_first(self, value: T) -> 'FunctionalArray[T]':
        return self.prepend(value)

    def insert_last(self, value: T) -> 'FunctionalArray[T]':
        return self.append(value)

    def delete_first(self) -> 'FunctionalArray[T]':
        return FunctionalArray(self.elements[1:])

    def delete_last(self) -> 'FunctionalArray[T]':
        return FunctionalArray(self.elements[:-1])

    def reverse(self) -> 'FunctionalArray[T]':
        return FunctionalArray(self.elements[::-1])

    def merge(self, other: 'FunctionalArray[T]') -> 'FunctionalArray[T]':
        return FunctionalArray(self.elements + other.elements)

    def find_by_index(self, index: int) -> Optional[T]:
        return self.get(index)

    @staticmethod
    def from_list(elements: List[T]) -> 'FunctionalArray[T]':
        return FunctionalArray(elements)

def generate_random_list(n: int, low: int, high: int) -> List[int]:
    return [random.randint(low, high) for _ in range(n)]

def measure_time_and_memory(func: Callable[[], T]) -> None:
    tracemalloc.start()
    start_time = time.time()
    start_memory = tracemalloc.get_traced_memory()[1]

    result = func()

    end_time = time.time()
    end_memory = tracemalloc.get_traced_memory()[1]
    tracemalloc.stop()

    elapsed_time = (end_time - start_time) * 1000  # Convert to milliseconds
    memory_used = end_memory - start_memory

    print(f"Execution time: {elapsed_time:.2f} ms")
    print(f"Memory used: {memory_used} bytes")

def run_memory_profiling() -> None:
    random_list = generate_random_list(10000, 1, 10000)
    functional_array = FunctionalArray.from_list(random_list)
    functional_array2 = FunctionalArray.from_list(random_list)

    print("Performance for operations:")
    measure_time_and_memory(lambda: FunctionalArray.from_list(random_list).fold_array(lambda acc, x: acc.append(x), FunctionalArray([])))
    print("Append profiling completed.")

    measure_time_and_memory(lambda: FunctionalArray.from_list(random_list).fold_array(lambda acc, x: acc.insert_first(x), FunctionalArray([])))
    print("InsertFirst profiling completed.")

    measure_time_and_memory(lambda: FunctionalArray.from_list(random_list).fold_array(lambda acc, x: acc.insert_last(x), FunctionalArray([])))
    print("InsertLast profiling completed.")

    measure_time_and_memory(lambda: functional_array.fold_array(lambda acc, _: acc.delete_first(), functional_array))
    print("DeleteFirst profiling completed.")

    measure_time_and_memory(lambda: functional_array.fold_array(lambda acc, _: acc.delete_last(), functional_array))
    print("DeleteLast profiling completed.")

    measure_time_and_memory(lambda: functional_array.reverse())
    print("Reverse profiling completed.")

    measure_time_and_memory(lambda: functional_array.merge(functional_array2))
    print("Merge profiling completed.")

    measure_time_and_memory(lambda: functional_array.find_by_index(5000))
    print("FindByIndex profiling completed.")

    measure_time_and_memory(lambda: functional_array.map_array(lambda x: x * 2))
    print("Map profiling completed.")

    measure_time_and_memory(lambda: functional_array.filter_array(lambda x: x % 2 == 0))
    print("Filter profiling completed.")

    measure_time_and_memory(lambda: functional_array.slice(5000, 6000))
    print("Slice profiling completed.")

    measure_time_and_memory(lambda: functional_array.length())
    print("Length profiling completed.")

    measure_time_and_memory(lambda: functional_array.get(5000))
    print("Get profiling completed.")

    measure_time_and_memory(lambda: functional_array.update(5000, 10000))
    print("Update profiling completed.")

    measure_time_and_memory(lambda: functional_array.fold_array(10000))
    print("Fold profiling completed.")

    measure_time_and_memory(lambda: FunctionalArray.prepend(random_list))
    print("Prepend profiling completed.")
    


if __name__ == "__main__":
    run_memory_profiling()