# University of Washington, Programming Languages, Homework 7, hw7.rb 
# (See also ML code)
#
# A little language for 2D geometry objects
#

class GeometryExpression
  Epsilon = 0.00001
end

class GeometryValue 
  private
  # some helper methods that may be generally useful
  def real_close(r1,r2) 
    (r1 - r2).abs < GeometryExpression::Epsilon
  end

  def real_close_point(x1,y1,x2,y2) 
    real_close(x1,x2) && real_close(y1,y2)
  end

  # two_points_to_line could return a Line or a VerticalLine
  def two_points_to_line(x1,y1,x2,y2) 
    if real_close(x1,x2)
      VerticalLine.new x1
    else
      m = (y2 - y1).to_f / (x2 - x1)
      b = y1 - m * x1
      Line.new(m,b)
    end
  end

  public
  # we put this in this class so all subclasses can inherit it:
  # the intersection of self with a NoPoints is a NoPoints object
  def intersectNoPoints np
    np # could also have NoPoints.new here instead
  end

  # we put this in this class so all subclasses can inhert it:
  # the intersection of self with a LineSegment is computed by
  # first intersecting with the line containing the segment and then
  # calling the result's intersectWithSegmentAsLineResult with the segment
  def intersectLineSegment seg
    line_result = intersect(two_points_to_line(seg.x1,seg.y1,seg.x2,seg.y2))
    line_result.intersectWithSegmentAsLineResult seg
  end

  # all values evaluate to themselves
  def eval_prog env
    self
  end
end

class NoPoints < GeometryValue
  # Note: no initialize method only because there is nothing it needs to do

  def preprocess_prog
    self # no pre-processing to do here
  end

  def shift(dx,dy)
    self # shifting no-points is no-points
  end

  def intersect other
    other.intersectNoPoints self # will be NoPoints but follow double-dispatch
  end

  def intersectPoint p
    self # intersection with point and no-points is no-points
  end

  def intersectLine line
    self # intersection with line and no-points is no-points
  end

  def intersectVerticalLine vline
    self # intersection with line and no-points is no-points
  end

  # if self is the intersection of (1) some shape s and (2) 
  # the line containing seg, then we return the intersection of the 
  # shape s and the seg.  seg is an instance of LineSegment
  def intersectWithSegmentAsLineResult seg
    self
  end
end


class Point < GeometryValue
  attr_reader :x, :y

  def initialize(x,y)
    @x = x
    @y = y
  end

  public
  def preprocess_prog
    self
  end

  def shift(dx,dy)
    Point.new(@x + dx, @y + dy)
  end

  def intersect other
    other.intersectPoint self
  end

  def intersectPoint p
    if real_close_point(p.x, p.y, @x, @y)
      p
    else
      NoPoints.new
    end
  end

  def intersectLine line
    if real_close(@y, line.m * @x + line.b)
      self
    else
      NoPoints.new
    end
  end

  def intersectVerticalLine vline
    if real_close(@x, vline.x)
      self
    else
      NoPoints.new
    end
  end

  def intersectWithSegmentAsLineResult seg
    # check if the point is on the segment and not just on the line containing the segment
    if inBetween(@x, seg.x1, seg.x2) && inBetween(@y, seg.y1, seg.y2)
      self
    else
      NoPoints.new
    end
  end

  private
  def inBetween(v, end1, end2)
    (end1 - GeometryExpression::Epsilon <= v && v <= end2 + GeometryExpression::Epsilon) || (end2 - GeometryExpression::Epsilon <= v && v <= end1 + GeometryExpression::Epsilon)
  end
end

class Line < GeometryValue
  attr_reader :m, :b 

  def initialize(m,b)
    @m = m
    @b = b
  end

  def preprocess_prog
    self
  end

  def shift(dx,dy)
    Line.new(@m, @b + dy - @m * dx)
  end

  def intersect other
    other.intersectLine self
  end

  def intersectPoint p
    p.intersectLine self
  end

  def intersectLine line
    if real_close(@m, line.m)
      if real_close(@b, line.b)
        line # same line
      else
        NoPoints.new # parallel lines never intersect
      end
    else
      # one point intersection
      x =  (line.b - @b) / (@m - line.m)
      y =  @m * x + @b
      Point.new(x, y)
    end
  end

  def intersectVerticalLine vline
    Point.new(vline.x, @m * vline.x + @b)
  end

  def intersectWithSegmentAsLineResult seg
    seg # the segment is on the line
  end
end

class VerticalLine < GeometryValue
  attr_reader :x

  def initialize x
    @x = x
  end

  def preprocess_prog
    self
  end

  def shift(dx,dy)
    VerticalLine.new(@x + dx)
  end

  def intersect other
    other.intersectVerticalLine self
  end

  def intersectPoint p
    p.intersectVerticalLine self
  end

  def intersectLine line
    line.intersectVerticalLine self
  end

  def intersectVerticalLine vline
    if real_close(@x, vline.x)
      vline # same line
    else
      NoPoints.new # parallel lines
    end
  end

  def intersectWithSegmentAsLineResult seg
    seg # the segment is on the vertical line
  end
end

class LineSegment < GeometryValue
  attr_reader :x1, :y1, :x2, :y2

  def initialize (x1,y1,x2,y2)
    @x1 = x1
    @y1 = y1
    @x2 = x2
    @y2 = y2
  end

  def preprocess_prog
    if real_close_point(@x1, @y1, @x2, @y2)
      Point.new(@x1, @y1)
    elsif @x1 > (@x2 + GeometryExpression::Epsilon) || (real_close(@x1, @x2) && @y1 > (@y2 + GeometryExpression::Epsilon))
      LineSegment.new(@x2, @y2, @x1, @y1)
    else
      self
    end
  end

  def shift(dx,dy)
    LineSegment.new(@x1 + dx, @y1 + dy, @x2 + dx, @y2 +dy)
  end

  def intersect other
    other.intersectLineSegment self
  end

  def intersectPoint p
    p.intersectLineSegment self
  end

  def intersectLine line
    line.intersectLineSegment self
  end

  def intersectVerticalLine vline
    vline.intersectLineSegment self
  end

  def intersectWithSegmentAsLineResult seg
    # check if the line segments are disjoint, overlapping, one inside the other, or just touching
    if real_close(seg.x1, seg.x2) # case of a vertical segment
      segments = seg.y1 < self.y1 ? [seg, self] : [self, seg] # segments[0] is below or right at segments[1]
      segment_below = segments[0]
      segment_above = segments[1]
      if real_close(segment_below.y2, segment_above.y1) # just touching
        Point.new(segment_below.x2, segment_below.y2)
      elsif segment_below.y2 < segment_above.y1 # disjoint
        NoPoints.new
      elsif segment_below.y2 > segment_above.y2 # above one inside below one
        LineSegment.new(segment_above.x1, segment_above.y1, segment_above.x2, segment_above.y2)
      else # overlapping
        LineSegment.new(segment_above.x1, segment_above.y1, segment_below.x2, segment_below.y2)
      end
    else # case of a non-vertical segment
      segments = seg.x1 < self.x1 ? [seg, self] : [self, seg] # segments[0] starts to the left of segments[1] or right at it
      segment_left = segments[0]
      segment_right = segments[1]
      if real_close(segment_left.x2, segment_right.x1) # just touching
        Point.new(segment_left.x2, segment_left.y2)
      elsif segment_left.x2 < segment_right.x1 # disjoint
        NoPoints.new
      elsif segment_left.x2 > segment_right.x2 # right one inside left one
        LineSegment.new(segment_right.x1, segment_right.y1, segment_right.x2, segment_right.y2)
      else # overlapping
        LineSegment.new(segment_right.x1, segment_right.y1, segment_left.x2, segment_left.y2)
      end
    end
  end
end

# Note: there might be no need for getter methods for the non-value classes
# but they are useful for test cases

class Intersect < GeometryExpression
  attr_reader :e1, :e2

  def initialize(e1,e2)
    @e1 = e1
    @e2 = e2
  end

  def eval_prog env
    eval_e1 = e1.eval_prog env
    eval_e2 = e2.eval_prog env
    eval_e1.intersect eval_e2
  end

  def preprocess_prog
    Intersect.new(@e1.preprocess_prog, @e2.preprocess_prog)
  end
end

class Let < GeometryExpression
  attr_reader :s, :e1, :e2

  def initialize(s,e1,e2)
    @s = s
    @e1 = e1
    @e2 = e2
  end

  def eval_prog env
    eval_e1 = @e1.eval_prog env
    new_env = env.clone.unshift([@s, eval_e1]) # at the beginning to properly handle shadowing
    e2.eval_prog new_env
  end

  def preprocess_prog
    Let.new(@s, @e1.preprocess_prog, @e2.preprocess_prog)
  end
end

class Var < GeometryExpression
  attr_reader :s

  def initialize s
    @s = s
  end

  def eval_prog env
    pr = env.assoc @s
    raise "undefined variable" if pr.nil?
    pr[1]
  end

  def preprocess_prog
    self
  end
end

class Shift < GeometryExpression
  attr_reader :dx, :dy, :e

  def initialize(dx,dy,e)
    @dx = dx
    @dy = dy
    @e = e
  end

  def eval_prog env
    eval_e = e.eval_prog env
    eval_e.shift(@dx, @dy)
  end

  def preprocess_prog
    Shift.new(@dx, @dy, @e.preprocess_prog)
  end
end
