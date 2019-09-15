<template id="salz-game-view">
  <div id="salz-game-inner-view"></div>
</template>

<style lang="scss">
#salz-game-inner-view {
  height: 100vh;
}
</style>

<script charset="utf-8">
export default {
  async mounted() {
    const PIXI = await import('pixi.js');
    const Viewport = await import('pixi-viewport');

    const wrapper = document.querySelector('#salz-game-inner-view');

    const appWidth = Math.max(
      document.documentElement.clientWidth,
      window.innerWidth || 0
    );
    const appHeight = Math.max(
      document.documentElement.clientHeight,
      window.innerHeight || 0
    );

    const app = new PIXI.Application({
      width: appWidth,
      height: appHeight,
      backgroundColor: 0x000000,
      resizeTo: document.querySelector('.main-content'),
      resolution: window.devicePixelRatio || 1
    });
    wrapper.appendChild(app.view);

    const vpWorldWidth = 1000;
    const vpWorldHeight = 1000;
    const viewport = new Viewport.Viewport({
      screenWidth: window.innerWidth,
      screenHeight: window.innerHeight,
      worldWidth: vpWorldWidth,
      worldHeight: vpWorldHeight,
      interaction: app.renderer.plugins.interaction
    });
    viewport.moveCenter(0, 0);
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
    window.addEventListener('resize', (event) => {
      app.resize();
    });
  }
};
</script>
