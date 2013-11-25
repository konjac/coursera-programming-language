# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
    All_My_Pieces = [
        [[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
        rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
        [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
        [[0, 0], [0, -1], [0, 1], [0, 2]]],
        rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
        rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
        rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
        rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
        [[[0, 0], [-1, 0], [1, 0], [2, 0], [-2, 0]],
         [[0, 0], [0, -1], [0, 1], [0, 2], [0, -2]]
        ], #longer 
        rotations([[0, 0], [0, -1], [1, 0], [1, -1], [2, 0]]), 
        rotations([[0, 0], [0, -1], [1, 0]])
        ]
  # your enhancements here
    
    Cheat_piece = [[[0, 0]]]
    
    # gets the next piece
    def next_piece(cheat=false)
        @current_block = MyPiece.next_piece(self, cheat)
        @current_pos = nil
    end  
    def self.next_piece (board, cheat=false)
        if cheat
            MyPiece.new(Cheat_piece, board)
        else
            MyPiece.new(All_My_Pieces.sample, board)
        end
    end  
end

class MyBoard < Board
  # your enhancements here
    def initialize (game)
        @cheat = false
        @grid = Array.new(num_rows) {Array.new(num_columns)}
        @current_block = MyPiece.next_piece(self)
        @score = 0
        @game = game
        @cheat = false
        @delay = 500
    end
    
    def set_cheat
        if !@cheat and @score >= 100
            @score = @score - 100
            @cheat = true
        end
    end
    
    def rotate_180_degree
        if !game_over? and @game.is_running?
            @current_block.move(0, 0, 1)
            @current_block.move(0, 0, 1)
        end
        draw
    end
    def next_piece
        @current_block = MyPiece.next_piece(self, @cheat)
        @cheat = false
        @current_pos = nil
    end
    
    def store_current
        locations = @current_block.current_rotation
        displacement = @current_block.position
        size = @current_block.current_rotation.size - 1
        (0..size).each{|index| 
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
    def initialize
        @root = TetrisRoot.new
        @timer = TetrisTimer.new
        set_board
        @running = true
        key_bindings
        buttons
        run_game
    end

    # creates a canvas and the board that interacts with it
    def set_board
        @canvas = TetrisCanvas.new
        @board = MyBoard.new(self)
        @canvas.place(@board.block_size * @board.num_rows + 3,
        @board.block_size * @board.num_columns + 6, 24, 80)
        @board.draw
    end
    
    def key_bindings 
        @root.bind('u', proc {@board.rotate_180_degree})
        @root.bind('c', proc {@board.set_cheat})
        super
    end
    
end


