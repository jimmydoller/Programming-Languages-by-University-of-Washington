# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  All_My_Pieces = All_Pieces.concat [rotations([[0, 0], [1, 0], [0, 1], [1, 1], [2, 1]]),
                                     rotations([[0, 0], [1, 0], [2, 0], [3, 0], [4, 0]]),
                                     rotations([[0, 0], [0, 1], [1, 0]])]

  def self.next_piece(board, cheat = false)
    MyPiece.new(cheat ? [[[0, 0]]] : All_My_Pieces.sample, board)
  end
end

class MyBoard < Board
  def initialize(game)
    super
    @current_block = MyPiece.next_piece(self)
    @cheat = false
  end

  def next_piece
    @current_block = MyPiece.next_piece(self, @cheat)
    @score -= 100 if @cheat
    @cheat = false
    @current_pos = nil
  end

  def cheat
    @cheat = !(@score < 100)
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    locations.each_index do |index|
      current = locations[index]
      @grid[current[1] + displacement[1]][current[0] + displacement[0]] = @current_pos[index]
    end
    remove_filled
    @delay = [@delay - 2, 80].max
  end
end

class MyTetris < Tetris
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind(
      'u',
      proc do
        @board.rotate_clockwise
        @board.rotate_clockwise
      end
    )
    @root.bind('c', proc { @board.cheat })
  end
end
