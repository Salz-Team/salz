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
    const activeCells = [];

    this.size = size;
    this.cells = activeCells;
  }

  /**
   * Iterates the grid to the next state
   * turnMoves format is [move], where move.x is x and move.y is y
   */
  next(turnMoves = []) {
    /*
     * Apply Player Commands
     */
    for (const move of turnMoves) {
      const filp = new Cell(move.x, move.y);
      filp.owner = move.owner;

      if (this.cells.filter(cell => filp.equals(cell))) {
        this.cells = this.cells.filter(cell => !filp.equals(cell));
      } else {
        this.cells.push(move);
      }
    }

    /*
     * Step Game of Life
     */
    const isAlive = cell => {
      return this.cells.filter(c => c.equals(cell)).length !== 0;
    };

    const getNeighboursOf = cell => {
      const neighbours = [];
      for (const i of [-1, 0, 1]) {
        for (const j of [-1, 0, 1]) {
          const newCell = new Cell(cell.x + i, cell.y + j);
          const gridCell = this.cells.filter(oldCell => oldCell.equals(newCell));

          if (gridCell.length !== 0) neighbours.push(...gridCell);
          else neighbours.push(newCell);
        }
      }

      return neighbours;
    };

    const getEmptyNeighboursOf = cell => {
      return getNeighboursOf(cell).filter(cell => !isAlive(cell));
    };

    const getAliveNeighboursOf = cell => {
      return getNeighboursOf(cell).filter(isAlive);
    };

    const isBorn = cell => {
      const numAliveNeighbours = getAliveNeighboursOf(cell).length;
      return numAliveNeighbours === 3;
    };

    const most = aliveNeighbours => {
      const playersCount = {};
      aliveNeighbours.forEach(neighbour => {
        const { owner } = neighbour;
        if (owner !== -1) playersCount[owner] = playersCount[owner] ? playersCount[owner] + 1 : 1;
      });

      // The game rules is such that players that join later have a priority
      // in gaining new cells, hence we need to reverse the array of keys
      return Object.keys(playersCount)
        .reverse()
        .reduce((a, b) => {
          return playersCount[a] > playersCount[b] ? a : b;
        });
    };

    const setOwner = cell => {
      const aliveNeighbours = getAliveNeighboursOf(cell);
      cell.owner = most(aliveNeighbours);
      return cell;
    };

    const isHealthy = cell => {
      const numAliveNeighbours = getAliveNeighboursOf(cell).length;
      return [2, 3].includes(numAliveNeighbours);
    };

    //                unique            ( neighbours                             )
    const emptyNeighbours = Array.from(new Set(this.cells.map(getEmptyNeighboursOf).flat()));

    const newCells = emptyNeighbours.filter(isBorn).map(setOwner);

    const stillAliveCells = this.cells.filter(isHealthy);

    // step turn
    this.cells = [...newCells, ...stillAliveCells];
  }

  mountSnapshot = snapshot => {
    snapshot.cells.forEach(cell => {
      const newCell = new Cell(cell.x, cell.y);
      newCell.owner = cell.playerid;
      this.cells.push(newCell);
    });
  };
}
