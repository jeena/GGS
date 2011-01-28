import unittest
from board import Board
from rectangle import Rectangle

class TestBoard(unittest.TestCase):
    def setUp(self):
        nr_of_rectangles = 4
        dimensions = Rectangle(0, 0, 25, 35)
        self.b = Board(nr_of_rectangles, dimensions)

    def test_rectangle_placemenets(self):
        #rectangle1: 0,0,25,35
        #rectangle2: 26,0,25,35
        #rectangle3: 0,36,25,35
        #rectangle4: 26,36,25,35

        r0 = self.b.game_rectangles[0]
        r1 = self.b.game_rectangles[1]
        r2 = self.b.game_rectangles[2]
        r3 = self.b.game_rectangles[3]
        self.assertEqual(r0.x, 0)
        self.assertEqual(r0.y, 0)
        self.assertEqual(r1.x, 25)
        self.assertEqual(r1.y, 0)
        self.assertEqual(r2.x, 0)
        self.assertEqual(r2.y, 35)
        self.assertEqual(r3.x, 25)
        self.assertEqual(r3.y, 35)


if __name__ == '__main__':
    unittest.main()
