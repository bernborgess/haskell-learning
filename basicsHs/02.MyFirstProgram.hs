-- 3. Define a function that takes a segment and returns
-- it's center point.
type Point = (Float, Float)
center :: (Point, Point) -> Point
center ((x, y), (x', y')) = ((x + x') / 2.0, (y + y') / 2.0)

r3 :: Point
r3 = center ((1, 2), (3, 4))