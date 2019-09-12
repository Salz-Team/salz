<template id="salz-game-view">
  <div id="salz-game-inner-view">
    Testing Pixi with Viewport plugin<br />
    scroll to zoom | drag<br />
  </div>
</template>

<script charset="utf-8">
export default {
  async mounted() {
    const PIXI = await import('pixi.js');
    const Viewport = await import('pixi-viewport');

    let type = 'WebGL';
    if (!PIXI.utils.isWebGLSupported()) {
      type = 'canvas';
    }

    PIXI.utils.sayHello(type);

    const app = new PIXI.Application({
      width: 500,
      height: 500,
      backgroundColor: 0x000000,
      resolution: window.devicePixelRatio || 1
    });
    document.querySelector('#salz-game-inner-view').appendChild(app.view);

    const viewport = new Viewport.Viewport({
      screenWidth: window.innerWidth,
      screenHeight: window.innerHeight,
      worldWidth: 1000,
      worldHeight: 1000,
      interaction: app.renderer.plugins.interaction
    });
    app.stage.addChild(viewport);
    viewport
      .drag()
      .pinch()
      .wheel()
      .decelerate();

    // This is a dummy data set
    const frame = [
      {
        playerid: 0,
        color: '0xff0000',
        coords: [{ x: 0, y: 1 }, { x: 20, y: 50 }, { x: 50, y: 20 }]
      },
      {
        playerid: 1,
        color: '0x00ff00',
        coords: [{ x: 50, y: 0 }, { x: 0, y: 50 }, { x: 20, y: 20 }]
      },
      {
        playerid: 2,
        color: '0x0000ff',
        coords: [{ x: -20, y: 0 }, { x: 0, y: -50 }, { x: 20, y: -20 }]
      }
    ];

    frame.forEach(function(player) {
      drawCells(player.coords, player.color);
    });

    function drawCells(coords, color) {
      coords.forEach(function(coord) {
        drawCell(coord, color);
      });
    }

    function drawCell(coord, color) {
      const sprite = viewport.addChild(new PIXI.Sprite(PIXI.Texture.WHITE));
      sprite.tint = color;
      sprite.width = sprite.height = 10;
      sprite.position.set(coord.x * 10, coord.y * 10);
    }
  }
};
</script>
