INSERT INTO LABSCHEMA.PLAYERS (
    PLAYER_NAME, TEAM_ID
) VALUES (
    'EDDIE THE EMBALMER',
    (SELECT TEAM_ID FROM LABSCHEMA.TEAMS
        WHERE TEAM_NAME = 'MORTICIANS')
);
INSERT INTO LABSCHEMA.PLAYERS (
    PLAYER_NAME, TEAM_ID
) VALUES (
    'CONNIE COFFINWRIGHT',
    (SELECT TEAM_ID FROM LABSCHEMA.TEAMS
        WHERE TEAM_NAME = 'MORTICIANS')
);
INSERT INTO LABSCHEMA.PLAYERS (
    PLAYER_NAME, TEAM_ID
) VALUES (
    'NANCY MANCER',
    (SELECT TEAM_ID FROM LABSCHEMA.TEAMS
        WHERE TEAM_NAME = 'NECROMANCERS')
);
INSERT INTO LABSCHEMA.PLAYERS (
    PLAYER_NAME, TEAM_ID
) VALUES (
    'KELLY KEELHAULER',
    (SELECT TEAM_ID FROM LABSCHEMA.TEAMS
        WHERE TEAM_NAME = 'SAILORS')
);
INSERT INTO LABSCHEMA.PLAYERS (
    PLAYER_NAME, TEAM_ID
) VALUES (
    'RONNIE RUDDERMAN',
    (SELECT TEAM_ID FROM LABSCHEMA.TEAMS
        WHERE TEAM_NAME = 'SAILORS')
);
INSERT INTO LABSCHEMA.PLAYERS (
    PLAYER_NAME, TEAM_ID
) VALUES (
    'ANNIE AMPHIBIAN',
    (SELECT TEAM_ID FROM LABSCHEMA.TEAMS
        WHERE TEAM_NAME = 'ZOOKEEPERS')
);
