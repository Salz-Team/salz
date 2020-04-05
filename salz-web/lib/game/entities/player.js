import Cell from './cell';

export default class Player {
  /**
   * Create player
   * @constructor
   * @param   {number}    id      ID of player
   * @param   {String}    color   Player color
   * @param   {Array}     cells   Array of positions for cells
   *
   * @return  {Player}
   */
  constructor(id, color, cells) {
    this.id = id;
    this.color = color;
    this.cells = [];

    cells.forEach(cell => {
      const cellObj = new Cell(id, cell.x, cell.y, color);
      this.cells.push(cellObj);
    });
  }
}
