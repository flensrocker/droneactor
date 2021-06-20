export type Dronetype = "worker" | "soldier" | "sniffer" | "infector" | "seeker";

export interface Coordinate {
  readonly q: number;
  readonly r: number;
}

export type FieldId = string;
export type DroneId = string;
export type PlayerId = string;

export interface HasId {
  readonly id: string;
}

export interface IdMap<TValue extends HasId> {
  readonly [id: string]: TValue;
}

export interface Drone extends HasId {
  readonly id: DroneId;
  readonly player_id: PlayerId;
  readonly type: Dronetype;
  readonly is_dead: boolean;
}

export interface Field extends HasId {
  readonly id: FieldId;
  readonly coordinate: Coordinate;
  readonly drones: IdMap<Drone>;
}

export interface Player extends HasId {
  readonly id: PlayerId;
  readonly player_name: string;
  readonly queen_field_id: FieldId;
  readonly queen_stress_level: number;
  readonly available_dronetypes: readonly Dronetype[];
  readonly current_dronetype: Dronetype;
  readonly next_dronetype: Dronetype;
}

export interface Game {
  readonly game_name: string;
  readonly game_size: number;
  readonly players: IdMap<Player>;
  readonly fields: IdMap<Field>;
}

export type MessageType =
  | "refresh_state"
  | "restart_game"
  | "change_dronetype"
  | "send_alarm"
  | "game_changed"
  | "player_changed"
  | "field_changed";

export interface SocketMessage<TMessage extends MessageType, TPayload> {
  readonly message: TMessage;
  readonly payload: TPayload;
}

export type RefreshStateCommand = SocketMessage<"refresh_state", null>;
export type RestartGameCommand = SocketMessage<"restart_game", null>;
export type ChangeDronetypeCommand = SocketMessage<"change_dronetype", Dronetype>;
export type SendAlarmCommand = SocketMessage<"send_alarm", null>;

export type GameChangedEvent = SocketMessage<"game_changed", Game>;
export type PlayerChangedEvent = SocketMessage<"player_changed", Player>;
export type FieldChangedEvent = SocketMessage<"field_changed", Field>;

// ----- Event Type Guards

export const isGameChange = (msg: SocketMessage<MessageType, unknown>): msg is GameChangedEvent => {
  return msg.message === "game_changed";
};

export const isPlayerChange = (msg: SocketMessage<MessageType, unknown>): msg is PlayerChangedEvent => {
  return msg.message === "player_changed";
};

export const isFieldChange = (msg: SocketMessage<MessageType, unknown>): msg is FieldChangedEvent => {
  return msg.message === "field_changed";
};

// ----- Helper

export const idmap_empty = <TValue extends HasId>(): IdMap<TValue> => {
  return {};
};

export const idmap_set = <TValue extends HasId>(map: IdMap<TValue>, value: TValue): IdMap<TValue> => {
  if (map[value.id] === value) {
    return map;
  } else {
    return {
      ...map,
      [value.id]: value,
    };
  }
};

export const idmap_delete = <TValue extends HasId>(map: IdMap<TValue>, id: string): IdMap<TValue> => {
  if (map[id] === undefined) {
    return map;
  } else {
    const { [id]: _, ...newMap } = map;
    return newMap;
  }
};
