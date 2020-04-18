let isFullscreen = false;
document.cancelFullScreen =
  document.cancelFullScreen || document.webkitCancelFullScreen || document.mozCancelFullScreen;

function onFullscreenChange() {
  isFullscreen = !isFullscreen;
}

export function fullscreen() {
  const el = document.querySelector('#salz-game-inner-view');
  el.addEventListener('webkitfullscreenchange', onFullscreenChange);
  el.addEventListener('mozfullscreenchange', onFullscreenChange);
  el.addEventListener('fullscreenchange', onFullscreenChange);

  if (!isFullscreen) {
    if (el.webkitRequestFullscreen) {
      el.webkitRequestFullScreen(Element.ALLOW_KEYBOARD_INPUT);
    } else {
      el.requestFullscreen();
    }
  } else {
    document.cancelFullScreen();
  }
}
