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
  data() {
    return {
      frames: []
    };
  },
  async asyncData(ctx) {
    // return {
    //   frames: await ctx.app.$framesRepo.frames()
    // };
  },
  async mounted() {
    const PIXI = await import('pixi.js');
    const Viewport = await import('pixi-viewport');

    document.cancelFullScreen =
      document.cancelFullScreen ||
      document.webkitCancelFullScreen ||
      document.mozCancelFullScreen;

    let isFullscreen = false;
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
      .decelerate()
      .clampZoom({
        minWidth: vpWorldWidth / 10,
        minHeight: vpWorldHeight / 10,
        maxWidth: vpWorldWidth * 10,
        maxHeight: vpWorldHeight * 10
      });

    // hotkeys
    document.onkeypress = function(e) {
      switch (e.keyCode) {
        case 102: // f
          fullscreen(app.view, !isFullscreen);
          break;
      }
    };

    const btn = createTextButton(
      'Fullscreen',
      { padding: 10, fill: '#FF0000' },
      () => {
        fullscreen(app.view, !isFullscreen);
      }
    );
    btn.x = appWidth - btn.width - 20;
    btn.y = appHeight - btn.height - 20;

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

    function fullscreen(el, value) {
      el.addEventListener('webkitfullscreenchange', onFullscreenChange);
      el.addEventListener('mozfullscreenchange', onFullscreenChange);
      el.addEventListener('fullscreenchange', onFullscreenChange);

      if (value) {
        if (el.webkitRequestFullscreen) {
          el.webkitRequestFullScreen(Element.ALLOW_KEYBOARD_INPUT);
        } else {
          el.mozRequestFullScreen();
        }
      } else {
        document.cancelFullScreen();
      }
    }

    function onFullscreenChange() {
      isFullscreen = !isFullscreen;
    }

    function createTextButton(text, style, fn) {
      const btn = new PIXI.Text(text, style);
      btn.interactive = true;
      btn.buttonMode = true;
      btn.on('click', fn);
      app.stage.addChild(btn);

      return btn;
    }

    window.addEventListener('resize', (event) => {
      app.resize();

      const appWidth = Math.max(
        document.documentElement.clientWidth,
        window.innerWidth || 0
      );
      const appHeight = Math.max(
        document.documentElement.clientHeight,
        window.innerHeight || 0
      );
      btn.x = appWidth - btn.width - 20;
      btn.y = appHeight - btn.height - 20;
    });
  }
};
</script>
