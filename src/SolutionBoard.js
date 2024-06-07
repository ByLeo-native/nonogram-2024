import './index.css';
import React from 'react';
import Square from './Square';

function SolutionBoard({ gridSolution, grid, revealSolution}) {
    const numOfRows = grid.length;
    const numOfCols = grid[0].length;

    return (
        <div>
            <p className='title-solution-grid'>Grilla resuelta</p>
            <div className={`vertical ${revealSolution ? '' : 'hide-solution-board'}`}>
                <div className="horizontal">
                    <div className={`board`}
                        style={{
                            gridTemplateRows: `repeat(${numOfRows}, 40px)`,
                            gridTemplateColumns: `repeat(${numOfCols}, 40px)`
                        }}>
                        {gridSolution.map((row, i) =>
                            row.map((cell, j) =>
                                <Square
                                    value={cell}
                                    
                                    key={i + j}
                                />
                            )
                        )}
                    </div>
                </div>
            </div>
        </div>
        
    );
}

export default SolutionBoard;