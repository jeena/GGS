import unittest
from rectangle import Rectangle



def main():
    x = 1
    y = 2
    width = 34
    height = 0.23
    r = Rectangle(x, y, width, height)

class TestRectangle(unittest.TestCase):
    def setUp(self):
        self.x = 1
        self.y = 2
        self.width = 34
        self.height = 0.23
        self.r = Rectangle(self.x, self.y, self.width, self.height)
    def test_attributes(self):
        self.assertEqual(self.x, self.r.x)
        self.assertEqual(self.y, self.r.y)
        self.assertEqual(self.r.width, self.r.width)
        self.assertEqual(self.r.height, self.r.height)

if __name__ == '__main__':
    unittest.main()
