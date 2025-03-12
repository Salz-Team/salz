export type GameHistoryMessageType = "gameStart" | "playerResponses" | "gameEnd";

export type GameHistoryLine = {
  gameType: string;
  messageType: GameHistoryMessageType;
  [_: string]: any;
};
