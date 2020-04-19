/**
 * @function Cell
 *
 * @param {number} x    x-coordinate of the cell
 * @param {number} y    y-coordinate of the cell
 *
 * @return
 * A cell in the grid of Conway's Game of Life
 * { x, y, isAlive: boolean, equals: Function }
 */
export class Cell {
  constructor(x, y) {
    this.owner = -1;
    this.x = x;
    this.y = y;
  }

  equals = cell => {
    return cell.x === this.x && cell.y === this.y;
  };
}

/**
 * @class Grid
 *
 * The playing field of Conway's Game of life
 */
export default class Grid {
  /**
   * @param {number} size    The width and height of grid
   */
  constructor(size) {
    const cells = [];

    this.size = size;
    this.cells = cells;
  }

  /**
   * Iterates the grid to the next state
   * turnMoves format is [move], where move.x is x and move.y is y
   */
  next(turnMoves = []) {
    /////////////////////////Apply Player Commands///////////////////////////////
    for (let move_ in turnMoves){
      const move = new Cell(move_.x, move_.y);
      move.owner = move_.owner;

      if (this.cells.filter(cell => move.equals(cell))){
        this.cells = this.cells.filter(cell => !move.equals(cell));
      } else {
        this.cells.push(move):
      }
    };


    /////////////////////////Step Game of Life///////////////////////////////////
    const isAlive = cell => {
      return this.cells.includes(cell);
    };

    const getEmptyNeighboursOf = cell => {
      return [for (x of [-1, 0, 1]) for (y of [-1, 0, 1]) new Cell(x, y)].filter(cell => !isAlive(cell));
    };

    const getAliveNeighboursOf = cell => {
      return [for (x of [-1, 0, 1]) for (y of [-1, 0, 1]) new Cell(x, y)].filter(isAlive);
    };

    const isBorn = cell => {
      const numAliveNeighbours = getAliveNeighboursOf(cell).length;
      if (numAliveNeighbours == 3){
        return true ;
      } else {
        return false ;
      }
    };

    const setOwner = cell => {
      const aliveNeighbours = getAliveNeighboursOf(cell);
      // most should return the pid that occures most in aliveNeighbours
      // if there is a tie, pick the highest pid
      cell.owner = most(aliveNeighbours);
      return cell
    };

    const isHealthy = cell => {
      const numAliveNeighbours = getAliveNeighboursOf(cell).length;
      if ( [2,3].includes(numAliveNeighbours) ){
        return true ;
      } else {
        return false ;
      }
    };


    //                unique            ( neighbours                             )
    const emptyNeighbours = Array.from(new Set( this.cells.map(getEmptyNeighboursOf).flat()));

    const newCells = emptyNeighbours.filter(isBorn).map(setOwner);


    const stillAliveCells = this.cells.filter(isHealthy)

    // step turn
    this.cells = [...newCells, ...stillAliveCells];
  }


  mountSnapshot = snapshot => {
    snapshot.cells.forEach(cell => {
      this.cells.forEach((gridCell, i) => {
        if (gridCell.equals(cell)) {
          this.cells[i].owner = cell.playerid;
          this.cells[i].isAlive = true;
          this.cells[i].nextState = true;
        }
      });
    });
  };
}
