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
    this.isAlive = false;
    this.nextState = false;
  }

  equals = cell => {
    return cell.x === this.x && cell.y === this.y;
  };

  iterate = () => {
    this.isAlive = this.nextState;
  };
}

/**
 * @class Grid
 *
 * The playing field of Conway's Game of life
 */
export default class Grid {
  /**
   * @param {number} w    The width of the grid
   * @param {number} h    The height of the grid
   */
  constructor(w, h) {
    const cells = [];
    for (let i = 1; i <= w; i++) {
      for (let j = 1; j <= h; j++) {
        cells.push(new Cell(i, j));
      }
    }

    this.w = w;
    this.h = h;
    this.cells = cells;
  }

  /**
   * Iterates the grid to the next state
   */
  next(turnMoves = []) {
    turnMoves.players.forEach(player => {
      player.moves.forEach(move => {
        const targetCellIndex = this.cells.findIndex(cell => cell.equals(move));
        if (targetCellIndex !== -1) {
          this.cells[targetCellIndex].isAlive = true;
          this.cells[targetCellIndex].nextState = true;
        }
      });
    });

    this.cells.forEach(cell => {
      const aliveNeighbours = this.getNeighboursOf(cell).filter(neighbour => neighbour.isAlive);

      if (cell.isAlive) {
        if (aliveNeighbours.length < 2 || aliveNeighbours.length > 3) cell.nextState = false;
        else cell.nextState = true;
      } else if (aliveNeighbours.length === 3) cell.nextState = true;
      else cell.nextState = false;
    });
    this.cells.forEach(cell => cell.iterate());
  }

  getNeighboursOf = cell => {
    return this.cells.filter(gridCell => {
      const diffx = Math.abs(gridCell.x - cell.x);
      const diffy = Math.abs(gridCell.y - cell.y);

      if (diffx === 0 && diffy === 0) return false;

      if (diffx <= 1 && diffy <= 1) return true;
    });
  };

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
