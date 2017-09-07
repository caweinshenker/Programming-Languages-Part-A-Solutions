# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece



  def initialize (point_array, board)
      super(point_array, board)
      @block_size = point_array[0].size
  end

  attr_accessor :block_size

  # The constant All_My_Pieces should be declared here
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
               rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
               [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
               [[0, 0], [0, -1], [0, 1], [0, 2]]],
               rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
               rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
               rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
               rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
               rotations([[0, 0], [1, 0], [0, 1], [1, 1], [2, 1]]), #2x3
               [[[0, 0], [-1, 0], [-2, 0], [1, 0], [2, 0]], #5x1
               [[0, 0], [0, -1], [0, -2], [0, 1], [0, 2]]], #5x1
               rotations([[0,0], [0, 1], [1,1]])] # corner

  # your enhancements here

  def self.next_piece (board)
      MyPiece.new(All_My_Pieces.sample, board)
  end

end


class MyBoard < Board
  attr_accessor :score, :cheats
  # your enhancements here

  def initialize (game)
      super(game)
      @current_block = MyPiece.next_piece(self)
      @cheats = 0
  end

  def next_piece
      if @cheats > 0
          @cheats -= 1
          @current_block = MyPiece.new([[0,0]], self)
          @current_pos = nil
      else
          @current_block = MyPiece.next_piece(self)
          @current_pos = nil
      end
  end

  def store_current
      locations = @current_block.current_rotation
      displacement = @current_block.position
      block_size = @current_block.block_size
      (0..block_size-1).each{|index|
        current = locations[index];
        @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
        @current_pos[index]
      }
      remove_filled
      @delay = [@delay - 2, 80].max
    end

end

class MyTetris < Tetris

  # your enhancements here

 def key_bindings
     @root.bind('u', proc {@board.rotate_clockwise; @board.rotate_clockwise})
     @root.bind('c', proc {if @board.score >= 100 and @board.cheats < 1
                                @board.score = @board.score - 100;
                                @board.cheats = @board.cheats + 1
                            end})
     super()
 end

 def set_board
     @canvas = TetrisCanvas.new
     @board = MyBoard.new(self)
     @canvas.place(@board.block_size * @board.num_rows + 3,
                   @board.block_size * @board.num_columns + 6, 24, 80)
     @board.draw
 end

end
