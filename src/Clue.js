function Clue({ clue, isComplete}) {

    return (
        <div className={`clue ${ isComplete ? 'clue-complete': ''}` }>
            {clue.map((num, i) =>
                <div key={i}>
                    {num}
                </div>
            )}
        </div>
    );
}

export default Clue;