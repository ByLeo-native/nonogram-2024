import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import ToggleSwitch from './ToggleSwitch';
import RevealButton from './RevealButton';
import WinnerBox from './WinnerBox';
import SolutionButton from './SolutionButton';
import SolutionBoard from './SolutionBoard';

let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [rowsClues, setRowsClues] = useState(null);
  const [colsClues, setColsClues] = useState(null);
  const [waiting, setWaiting] = useState(false);
  const [rowsCluesState, setRowsCluesState] = useState(null);
  const [colsCluesState, setColsCluesState] = useState(null);
  const [isPaintedMode, setIsPaintedMode] = useState(true);
  const [winner, setWinner] = useState(null);

  const [helpButtons, setHelpButtons] = useState(false);
  const [revealMode, setRevealMode] = useState(false);
  const [showSolution, setShowSolution] = useState(false);
  const [solutionGrid, setSolutionGrid] = useState(null);
  const [statusText, setStatusText] = useState('Keep playing!');

  useEffect(() => {
    // Creation of the pengine server instance.    
    // This is executed just once, after the first render.    
    // The callback will run when the server is ready, and it stores the pengine instance in the pengine variable. 
    PengineClient.init(handleServerReady);  
  }, []);

  function handleServerReady(instance) {
    pengine = instance;
    let queryS = 'init(RowClues, ColumClues, Grid)';
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setRowsClues(response['RowClues']);
        setColsClues(response['ColumClues']);
        setRowsCluesState(Array(response['RowClues'].length).fill(false));
        setColsCluesState(Array(response['ColumClues'].length).fill(false));
        
        /**
         * Se realiza una nueva peticion para verificar si el nonograma cuenta con alguna restricción cumplida.
         * Se utiliza la respuesta de la primera peticion en vez de las constantes declaradas porque falla.
         */
        const g = JSON.stringify(response['Grid']).replaceAll('"_"', '_');
        const rCluesS = JSON.stringify(response['RowClues']);
        const cCluesS = JSON.stringify(response['ColumClues']);
        const querySS = `check_clues(${g}, ${rCluesS}, ${cCluesS}, StatusOfRows, StatusOfCols)`;
        setWaiting(true);
        pengine.query(querySS, (success, response) => {
          if(success) {
            setRowsCluesState(response['StatusOfRows'].map(element => {return element === 1 ? true : false;}));
            setColsCluesState(response['StatusOfCols'].map(element => {return element === 1 ? true : false;}));
          } else {
            console.error(`Error cuando se solicita check_clues`);
            console.log(response);
          }
          setWaiting(false);
        })
        
        // Ver si el nonograma esta inicialmente resuelto
        const querySSS = `solve(${g}, ${rCluesS}, ${cCluesS}, Solved)`;
        pengine.query(querySSS, (success, response) => {
          if(success) {
            if(response['Solved']) {
              setStatusText(``);
              setWinner(`¡Has completado el nonograma!`);
            }
          } else {
            console.error(`La solicitud de solve no fue exitosa`);
          }
        });
        
        // RESOLVER NONOGRAMA: 
        const querySSSS = `resolver_nonograma(${rCluesS}, ${cCluesS}, SolutionGrid)`;
        pengine.query(querySSSS, (success, response) => {
          if(success) {
            setSolutionGrid(response['SolutionGrid']);
          }
        })
        setHelpButtons(true);
      }
    });
  }

  function updateClues(row, col, RowSat, ColSat) {
    const updatedRowsCluesState = [...rowsCluesState];
    // Aquí actualiza las Clues en fila en función de RowSat
    updatedRowsCluesState[row] = RowSat === 1;
    // Actualiza el estado de las Clues en columna
    const updatedColsCluesState = [...colsCluesState];
    // Aquí actualiza las Clues en columna en función de ColSat
    updatedColsCluesState[col] = ColSat === 1;
    setRowsCluesState(updatedRowsCluesState);
    setColsCluesState(updatedColsCluesState);
  }

  function handleClick(i, j) {
    // No action on click if we are waiting.
    if (waiting) {
      return;
    }
    // En caso de que se este mostrando la solución de la grilla, entonces no se permitira la interacción
    if(showSolution) {
      return;
    } else {
      if(!(helpButtons && revealMode)) {
        // Funcionamiento normal, cuando el boton revealMode esta desactivado  
        const squaresS = JSON.stringify(grid).replaceAll('"_"', '_'); // Remove quotes for variables. squares = [["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]]
        const rowsCluesS = JSON.stringify(rowsClues);
        const colsCluesS = JSON.stringify(colsClues);
  
        const content = isPaintedMode ? '#' : 'X';
        const queryS = `put("${content}", [${i},${j}], ${rowsCluesS}, ${colsCluesS}, ${squaresS}, ResGrid, RowSat, ColSat)`; // queryS = put("#",[0,1],[], [],[["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]], GrillaRes, FilaSat, ColSat)
        setWaiting(true);
        pengine.query(queryS, (success, response) => {
          if (success) {
            setGrid(response['ResGrid']);
            updateClues(i, j, response['RowSat'], response['ColSat']);
            /**
             * Si se verifica las restricciones en fila y en columna en simultaneo, entonces se verifica si se resolvio el nonograma.
             * VERIFICACION DE NONOGRAMA
             */
            if (response['RowSat'] === 1 && response['ColSat'] === 1) {
              const g = JSON.stringify(response['ResGrid']).replaceAll('"_"', '_');
              const querySS = `solve(${g}, ${rowsCluesS}, ${colsCluesS}, Solved)`;
              pengine.query(querySS, (success, response) => {
                if(success) {
                  if(response['Solved']) {
                    setStatusText(`¡Has completado el nonograma!`);
                    setWinner(statusText);
                  } else {
                    console.log(`No esta resuelto`);
                  }
                } else {
                  console.error(`La solicitud de solve no fue exitosa`);
                }
              });
            }
            setWaiting(false);
          }
        })
      } else {
        // REVELAR CELDA: Funcionamiento para revelar la solucion de una celda del nonograma
        const rowsCluesS = JSON.stringify(rowsClues);
        const colsCluesS = JSON.stringify(colsClues);
  
        const queryS = `revelar_celda([${i},${j}], ${rowsCluesS}, ${colsCluesS}, RevealedContent)`;
        setWaiting(true);
        pengine.query(queryS, (success, response) => {
          if (success) {
            console.log(`${response['RevealedContent']}`);
            const newGrid = [...grid];
            newGrid[i][j] = response['RevealedContent'];
            setGrid(newGrid);
          } else {
            console.error(`Error en la consulta revelar ${response}`)
          }
          setWaiting(false);
        });
        setRevealMode(!revealMode);
      }
    }
  }

  if (!grid) {
    return null;
  }
  
  return (
    <div className="game">
      <div className="container">
        <RevealButton
          revealMode={revealMode}
          setRevealMode={setRevealMode}
          helpsActive={helpButtons}
        />
        <SolutionButton
          solutionMode={showSolution}
          setSolutionMode={setShowSolution}
          helpsActive={helpButtons}
        />
      </div>
      <div className='boards-container'>
        <Board
          grid={grid}
          rowsClues={rowsClues}
          colsClues={colsClues}
          onClick={(i, j) => handleClick(i, j)}
          rowsCluesState={rowsCluesState}
          colsCluesState={colsCluesState}
          blockGrid={showSolution}
        />

        {showSolution && solutionGrid && (
          <SolutionBoard
            gridSolution={solutionGrid}
            revealSolution={showSolution}
          />
        )}
      </div>
      

      <ToggleSwitch 
        paintedMode={isPaintedMode}
        setPaintedMode={setIsPaintedMode}
      />
      <div className="game-info">
        {statusText}
      </div>
      <WinnerBox 
        winner={winner}
      />
    </div>
  );
}

export default Game;