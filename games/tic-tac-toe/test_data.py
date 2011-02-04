import unittest
import data

    
class TestData(unittest.TestCase):
    def setUp(self):
        array = [0,1,1,1,3,4,5,2,2,3,3,3,3,3,33,4,2,2]

        self.assertTrue(data.greatest_sequence(array, 3) == 5)

if __name__ == '__main__':
    unittest.main()


