export type Colourscheme = {
  black: string;
  white: string;
  lightgray: string;
  darkgray: string;
  brightRed: string;
  brightGreen: string;
  brightYellow: string;
  brightBlue: string;
  brightPurple: string;
  brightOrange: string;
  darkRed: string;
  darkGreen: string;
  darkYellow: string;
  darkBlue: string;
  darkPurple: string;
  darkOrange: string;
  foreground: string;
  background: string;
};

export function getColourscheme(htmlElement: HTMLElement): Colourscheme {
  // Note that JS doesn't like it if you do this:
  // const getPropertyValue = getComputedStyle(htmlElement).getPropertyValue;
  const getPropertyValue = (property: string) =>
    getComputedStyle(htmlElement).getPropertyValue(property);

  return {
    black: getPropertyValue('--colour-black'),
    white: getPropertyValue('--colour-white'),
    lightgray: getPropertyValue('--colour-lightgray'),
    darkgray: getPropertyValue('--colour-darkgray'),
    brightRed: getPropertyValue('--colour-bright-red'),
    brightGreen: getPropertyValue('--colour-bright-green'),
    brightYellow: getPropertyValue('--colour-bright-yellow'),
    brightBlue: getPropertyValue('--colour-bright-blue'),
    brightPurple: getPropertyValue('--colour-bright-purple'),
    brightOrange: getPropertyValue('--colour-bright-orange'),
    darkRed: getPropertyValue('--colour-dark-red'),
    darkGreen: getPropertyValue('--colour-dark-green'),
    darkYellow: getPropertyValue('--colour-dark-yellow'),
    darkBlue: getPropertyValue('--colour-dark-blue'),
    darkPurple: getPropertyValue('--colour-dark-purple'),
    darkOrange: getPropertyValue('--colour-dark-orange'),
    foreground: getPropertyValue('--colour-foreground'),
    background: getPropertyValue('--colour-background'),
  } as Colourscheme;
}

