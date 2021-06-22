import { Component } from '@angular/core';
import { PlayerSocketClient } from './player-socket-client';

@Component({
  selector: 'fest-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css'],
})
export class AppComponent {
  title = 'drone-actor';

  constructor(private _playerSocket: PlayerSocketClient) {

  }
}
