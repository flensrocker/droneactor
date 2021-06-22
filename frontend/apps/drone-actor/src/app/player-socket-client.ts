import { Injectable, OnDestroy } from '@angular/core';
import { Observer, Subscription } from 'rxjs';
import { webSocket, WebSocketSubject } from 'rxjs/webSocket';

import { MessageType, SocketMessage } from './messages';

@Injectable({
    providedIn: 'root'
})
export class PlayerSocketClient implements OnDestroy {
    private readonly _url = '/api/player/connect';
    
    private _socket: WebSocketSubject<SocketMessage<MessageType, unknown>> | null = null;
    private _heartbeat: Subscription | null = null;

    connect(messageObserver: Observer<SocketMessage<MessageType, unknown>>): Subscription {
        if (this._socket === null) {
            this._socket = webSocket<SocketMessage<MessageType, unknown>>(this._url);
            this._heartbeat = this._socket.subscribe({
                error: (err) => {
                    this._socket = null;
                    this._heartbeat = null;
                },
                complete: () => {
                    this._socket = null;
                    this._heartbeat = null;
                }
            });
        }

        return this._socket.subscribe(messageObserver);
    }

    ngOnDestroy(): void {
        if (this._heartbeat !== null) {
            this._heartbeat.unsubscribe();
        }
        if (this._socket !== null) {
            this._socket.complete();
            this._socket = null;
        }
    }

    send(message: SocketMessage<MessageType, unknown>): boolean {
        if (this._socket === null) {
            return false;
        }

        this._socket.next(message);
        return true;
    }
}
