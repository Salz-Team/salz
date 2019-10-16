import Cell from '../entities/cell';
import CellInfoBox from '../ui/cellInfoBox';

export default class Frame {
  constructor(viewport, frame) {
    this.viewport = viewport;
    this.frame = frame;
  }

  /**
   * Void function to draw the frame
   */
  draw() {
    this.frame.forEach((player) => {
      player.pos.forEach((position) => {
        const cell = new Cell(
          player.playerid,
          position.x,
          position.y,
          player.color
        );

        cell.on('mouseover', (event) => {
          let cellInfoBox;
          const delay = setTimeout(() => {
            const oriEvent = event.data.originalEvent;
            const cellInfoObj = {
              owner: cell.owner,
              x: cell.cellx,
              y: cell.celly
            };
            cellInfoBox = new CellInfoBox(
              oriEvent.clientX,
              oriEvent.clientY,
              cellInfoObj
            );
            document
              .querySelector('#salz-game-inner-view')
              .appendChild(cellInfoBox);
          }, 200);

          // once we leave the cell, remove cellInfoBox
          // only do this if cellInfoBox was created
          cell.on('mouseout', () => {
            clearTimeout(delay);
            if (typeof cellInfoBox !== 'undefined') {
              setTimeout(() => {
                document
                  .querySelector('#salz-game-inner-view')
                  .removeChild(cellInfoBox);
              }, 100);
            }

            // This is to ensure not we don't end up with
            // multiple mouseout events that have no relationship
            // with one another
            cell.removeListener('mouseout');
          });
        });

        this.viewport.addChild(cell);
      });
    });
  }
}
