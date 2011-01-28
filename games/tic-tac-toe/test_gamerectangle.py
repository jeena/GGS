import unittest
from gamerectangle import GameRectangle



class TestGameRectangle(unittest.TestCase):
    def setUp(self):
        self.index = 0
        self.x = 1
        self.y = 2
        self.width = 34
        self.height = 0.23
        self.gr = GameRectangle(self.index, self.x, self.y, self.width, self.height)
    def test_attributes(self):
        self.assertEqual(self.index, self.gr.index)
        self.assertEqual(self.gr.state, ' ')
        self.assertEqual(self.x, self.gr.x)
        self.assertEqual(self.y, self.gr.y)
        self.assertEqual(self.width, self.gr.width)
        self.assertEqual(self.height, self.gr.height)

if __name__ == '__main__':
    unittest.main()
