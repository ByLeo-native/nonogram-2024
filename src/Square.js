import React from 'react';

function Square({ value, onClick }) {
    return (
        <button className={`square ${(value === '#') ? 'square-painted' : null}`} onClick={onClick}>
            {value !== '_' ? value : null}
        </button>
    );
}

export default Square;